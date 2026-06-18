*&---------------------------------------------------------------------*
*& Report Z_EASY_AI
*&---------------------------------------------------------------------*
*& Easy AI API example with GUI popup.
*& Calls the LLM API directly via create_by_url (no SM59 destination).
*& Provider is chosen from a listbox (Anthropic / Mistral URL); the model
*& is picked from a dropdown filled live from <base>/v1/models.
*&---------------------------------------------------------------------*
REPORT z_easy_ai.

TYPES ty_prov TYPE c LENGTH 12.

SELECTION-SCREEN BEGIN OF BLOCK b_api WITH FRAME TITLE TEXT-001.
PARAMETERS: p_prov   TYPE ty_prov  AS LISTBOX VISIBLE LENGTH 30
                     USER-COMMAND prov DEFAULT 'ANTHROPIC',     " provider (holds base URL)
            p_model  TYPE text255  AS LISTBOX VISIBLE LENGTH 45 MEMORY ID model,
            p_apikey TYPE text255  MEMORY ID api.
SELECTION-SCREEN END OF BLOCK b_api.

SELECTION-SCREEN BEGIN OF BLOCK b_files WITH FRAME TITLE TEXT-002.
PARAMETERS: p_folder TYPE text255 DEFAULT 'C:\soft\GitHub\ABAP-AI-Code\TOOLS',
            p_file   TYPE text255 AS LISTBOX VISIBLE LENGTH 50
                     USER-COMMAND file.
SELECTION-SCREEN END OF BLOCK b_files.

" Remembers provider+key the model list was built for (avoid re-fetch on Enter).
DATA gv_loaded_key TYPE string.
" Cached model listbox values - re-applied on every PBO so the list persists.
DATA gt_model_vrm    TYPE vrm_values.
DATA gv_loaded_folder TYPE string.
DATA gt_file_vrm      TYPE vrm_values.

*----------------------------------------------------------------------*
* lcl_ai_api - direct HTTP communication with the LLM API (no SM59)
*----------------------------------------------------------------------*
CLASS lcl_ai_api DEFINITION.
  PUBLIC SECTION.
    TYPES tt_ids TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    " Base URL for a provider code (ANTHROPIC / MISTRAL).
    CLASS-METHODS base_url
      IMPORTING i_prov       TYPE clike
      RETURNING VALUE(rv_url) TYPE string.

    " 'ANTHROPIC' for Anthropic, otherwise 'OPENAI' (Mistral is OpenAI-compatible).
    CLASS-METHODS provider_of
      IMPORTING i_prov            TYPE clike
      RETURNING VALUE(rv_provider) TYPE string.

    CLASS-METHODS ask
      IMPORTING i_prompt         TYPE string
                i_base_url       TYPE string
                i_model          TYPE text255
                i_apikey         TYPE string
                i_provider       TYPE string DEFAULT 'ANTHROPIC'
                i_json_schema    TYPE string OPTIONAL
      RETURNING VALUE(rv_answer) TYPE string.

    " GET <base>/v1/models -> list of model ids.
    CLASS-METHODS list_models
      IMPORTING i_base_url TYPE string
                i_apikey   TYPE string
                i_provider TYPE string
      EXPORTING et_ids     TYPE tt_ids
                e_error    TYPE string.

  PRIVATE SECTION.
    CLASS-METHODS:
      build_payload
        IMPORTING i_prompt        TYPE string
                  i_model         TYPE text255
                  i_provider      TYPE string
                  i_json_schema   TYPE string OPTIONAL
        RETURNING VALUE(rv_json)  TYPE string,

      parse_response
        IMPORTING i_json           TYPE string
                  i_provider       TYPE string
        RETURNING VALUE(rv_answer) TYPE string,

      " Sets Content-Type + provider auth headers on the request.
      set_auth_headers
        IMPORTING io_client  TYPE REF TO if_http_client
                  i_provider TYPE string
                  i_apikey   TYPE string.
ENDCLASS.

CLASS lcl_ai_api IMPLEMENTATION.

  METHOD base_url.
    DATA(lv_prov) = CONV string( i_prov ).
    TRANSLATE lv_prov TO UPPER CASE.
    rv_url = SWITCH #( lv_prov
                       WHEN 'MISTRAL' THEN 'https://api.mistral.ai'
                       WHEN 'OPENAI'  THEN 'https://api.openai.com'
                       ELSE 'https://api.anthropic.com' ).
  ENDMETHOD.

  METHOD provider_of.
    DATA(lv_prov) = CONV string( i_prov ).
    TRANSLATE lv_prov TO UPPER CASE.
    rv_provider = COND #( WHEN lv_prov = 'ANTHROPIC' THEN 'ANTHROPIC' ELSE 'OPENAI' ).
  ENDMETHOD.

  METHOD set_auth_headers.
    io_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
    IF i_provider = 'ANTHROPIC'.
      io_client->request->set_header_field( name = 'anthropic-version' value = '2023-06-01' ).
      io_client->request->set_header_field( name = 'x-api-key'         value = i_apikey ).
    ELSE.
      DATA(lv_auth) = i_apikey.
      IF lv_auth CP 'Bearer *' OR lv_auth CP 'bearer *'.
        io_client->request->set_header_field( name = 'Authorization' value = lv_auth ).
      ELSE.
        io_client->request->set_header_field( name = 'Authorization' value = |Bearer { lv_auth }| ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD ask.
    DATA o_client TYPE REF TO if_http_client.

    DATA(lv_provider) = i_provider.
    TRANSLATE lv_provider TO UPPER CASE.
    IF lv_provider IS INITIAL.
      lv_provider = 'ANTHROPIC'.
    ENDIF.

    " Anthropic: /v1/messages ; OpenAI-compatible (Mistral): /v1/chat/completions
    DATA(lv_url) = i_base_url &&
      COND string( WHEN lv_provider = 'ANTHROPIC'
                   THEN '/v1/messages'
                   ELSE '/v1/chat/completions' ).

    cl_http_client=>create_by_url(
      EXPORTING url    = lv_url
                ssl_id = 'ANONYM'
      IMPORTING client = o_client
      EXCEPTIONS OTHERS = 4 ).
    IF sy-subrc <> 0.
      rv_answer = |Error: create_by_url failed rc={ sy-subrc } (check URL / SSL in STRUST)|.
      RETURN.
    ENDIF.

    set_auth_headers( io_client = o_client i_provider = lv_provider i_apikey = i_apikey ).
    o_client->request->set_method( 'POST' ).
    o_client->request->set_cdata( build_payload(
      i_prompt      = i_prompt
      i_model       = i_model
      i_provider    = lv_provider
      i_json_schema = i_json_schema ) ).
    " Suppress the SAP logon popup so a 401/403 returns the JSON error body.
    o_client->propertytype_logon_popup = if_http_client=>co_disabled.

    o_client->send( EXCEPTIONS http_communication_failure = 1 OTHERS = 5 ).
    IF sy-subrc <> 0.
      rv_answer = 'Error: HTTP send failed'.
      RETURN.
    ENDIF.
    o_client->receive( EXCEPTIONS http_communication_failure = 1 OTHERS = 4 ).

    rv_answer = parse_response(
      i_json     = o_client->response->get_cdata( )
      i_provider = lv_provider ).
  ENDMETHOD.

  METHOD list_models.
    DATA o_client TYPE REF TO if_http_client.

    CLEAR: et_ids, e_error.

    DATA(lv_provider) = i_provider.
    TRANSLATE lv_provider TO UPPER CASE.

    cl_http_client=>create_by_url(
      EXPORTING url    = |{ i_base_url }/v1/models|
                ssl_id = 'ANONYM'
      IMPORTING client = o_client
      EXCEPTIONS OTHERS = 4 ).
    IF sy-subrc <> 0.
      e_error = |create_by_url failed rc={ sy-subrc }|.
      RETURN.
    ENDIF.

    set_auth_headers( io_client = o_client i_provider = lv_provider i_apikey = i_apikey ).
    o_client->request->set_method( 'GET' ).
    o_client->propertytype_logon_popup = if_http_client=>co_disabled.

    o_client->send( EXCEPTIONS http_communication_failure = 1 OTHERS = 2 ).
    IF sy-subrc <> 0.
      e_error = 'HTTP send failed'.
      RETURN.
    ENDIF.
    o_client->receive( EXCEPTIONS http_communication_failure = 1 OTHERS = 2 ).
    IF sy-subrc <> 0.
      e_error = 'HTTP receive failed'.
      RETURN.
    ENDIF.

    DATA(lv_json) = o_client->response->get_cdata( ).

    " Both Anthropic and OpenAI-compatible (Mistral/OpenAI) APIs return models as
    " data[].id. Extract every "id":"..." directly - a string scan is immune to
    " the structure/field quirks that can make /ui2/cl_json return an empty table
    " on large model lists. Only the model objects carry an "id" key.
    DATA lv_off  TYPE i.
    DATA lv_from TYPE i.
    DATA lv_id   TYPE string.
    DO.
      FIND FIRST OCCURRENCE OF REGEX '"id"\s*:\s*"([^"]*)"'
        IN SECTION OFFSET lv_from OF lv_json
        SUBMATCHES lv_id
        MATCH OFFSET lv_off.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      IF lv_id IS NOT INITIAL.
        APPEND lv_id TO et_ids.
      ENDIF.
      lv_from = lv_off + 5.   " advance past this "id" marker
    ENDDO.

    IF et_ids IS INITIAL.
      DATA(lv_len) = nmin( val1 = strlen( lv_json ) val2 = 150 ).
      e_error = |Unexpected response: { lv_json(lv_len) }|.
    ENDIF.
  ENDMETHOD.

  METHOD build_payload.
    DATA: lv_prompt   TYPE string,
          lv_provider TYPE string,
          lv_cr       TYPE c LENGTH 1.

    lv_provider = i_provider.
    TRANSLATE lv_provider TO UPPER CASE.
    lv_cr = cl_abap_char_utilities=>cr_lf(1).

    lv_prompt = i_prompt.
    REPLACE ALL OCCURRENCES OF '\' IN lv_prompt WITH '\\'.
    REPLACE ALL OCCURRENCES OF '"' IN lv_prompt WITH '\"'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_prompt WITH '\n'.
    REPLACE ALL OCCURRENCES OF lv_cr IN lv_prompt WITH '\r'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_prompt WITH '\n'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN lv_prompt WITH '\f'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN lv_prompt WITH '\t'.

    " response_format (json_schema) is OpenAI-only - Anthropic ignores it
    DATA lv_response_format TYPE string.
    IF i_json_schema IS NOT INITIAL AND lv_provider = 'OPENAI'.
      lv_response_format = |, "response_format": { i_json_schema }|.
    ENDIF.

    rv_json = |{ '{' }"model": "{ i_model }", "messages": [{ '{' }"role": "user", "content": "{ lv_prompt }"{ '}' }], "max_tokens": 2000{ lv_response_format }{ '}' }|.
  ENDMETHOD.

  METHOD parse_response.
    TYPES: BEGIN OF t_prompt_tokens_details,
             cached_tokens TYPE string,
           END OF t_prompt_tokens_details,
           BEGIN OF t_usage,
             prompt_tokens         TYPE string,
             completion_tokens     TYPE string,
             total_tokens          TYPE string,
             prompt_tokens_details TYPE t_prompt_tokens_details,
             input_tokens          TYPE string,
             output_tokens         TYPE string,
           END OF t_usage,
           BEGIN OF t_content_block,
             type TYPE string,
             text TYPE string,
           END OF t_content_block,
           t_content_blocks TYPE STANDARD TABLE OF t_content_block WITH NON-UNIQUE DEFAULT KEY,
           BEGIN OF t_anthropic_res,
             id          TYPE string,
             type        TYPE string,
             role        TYPE string,
             model       TYPE string,
             stop_reason TYPE string,
             content     TYPE t_content_blocks,
             usage       TYPE t_usage,
           END OF t_anthropic_res.

    TYPES: BEGIN OF t_openai_message,
             role              TYPE string,
             content           TYPE string,
             reasoning_content TYPE string,
           END OF t_openai_message,
           BEGIN OF t_openai_choice,
             index         TYPE string,
             message       TYPE t_openai_message,
             finish_reason TYPE string,
           END OF t_openai_choice,
           t_openai_choices TYPE STANDARD TABLE OF t_openai_choice WITH NON-UNIQUE DEFAULT KEY,
           BEGIN OF t_openai_res,
             id      TYPE string,
             object  TYPE string,
             created TYPE string,
             model   TYPE string,
             choices TYPE t_openai_choices,
             usage   TYPE t_usage,
           END OF t_openai_res.

    DATA: lv_provider     TYPE string,
          response        TYPE t_anthropic_res,
          openai_response TYPE t_openai_res,
          lv_text         TYPE string,
          lv_usage_info   TYPE string.

    lv_provider = i_provider.
    TRANSLATE lv_provider TO UPPER CASE.

    IF lv_provider = 'OPENAI'.
      /ui2/cl_json=>deserialize( EXPORTING json = i_json CHANGING data = openai_response ).
      IF openai_response-choices IS NOT INITIAL.
        lv_text = openai_response-choices[ 1 ]-message-content.
        IF openai_response-usage-total_tokens IS NOT INITIAL.
          lv_usage_info = |Tokens: prompt={ openai_response-usage-prompt_tokens } completion={ openai_response-usage-completion_tokens } total={ openai_response-usage-total_tokens } cached={ openai_response-usage-prompt_tokens_details-cached_tokens }|.
          rv_answer = lv_text && cl_abap_char_utilities=>newline && cl_abap_char_utilities=>newline && lv_usage_info.
        ELSE.
          rv_answer = lv_text.
        ENDIF.
      ELSE.
        rv_answer = i_json.
      ENDIF.
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_json CHANGING data = response ).

    IF response-content IS NOT INITIAL.
      lv_text = response-content[ 1 ]-text.
      IF response-usage-input_tokens IS NOT INITIAL.
        lv_usage_info = |Tokens: input={ response-usage-input_tokens } output={ response-usage-output_tokens }|.
        rv_answer = lv_text && cl_abap_char_utilities=>newline && cl_abap_char_utilities=>newline && lv_usage_info.
      ELSE.
        rv_answer = lv_text.
      ENDIF.
    ELSE.
      rv_answer = i_json.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* lcl_html - markdown -> HTML renderer (ported from ZCL_CODE_HTML_GEN)
*----------------------------------------------------------------------*
CLASS lcl_html DEFINITION.
  PUBLIC SECTION.
    " Wraps the rendered markdown body in a full HTML document with CSS.
    CLASS-METHODS answer_to_html
      IMPORTING i_answer       TYPE string
                i_source       TYPE string OPTIONAL
                i_title        TYPE string OPTIONAL
      RETURNING VALUE(rv_html) TYPE string.

  PRIVATE SECTION.
    CLASS-METHODS render_abap_blocks
      IMPORTING i_text         TYPE string
      RETURNING VALUE(rv_text) TYPE string.
    CLASS-METHODS render_markdown_text
      IMPORTING i_text         TYPE string
      RETURNING VALUE(rv_html) TYPE string.
    CLASS-METHODS render_inline_markdown
      IMPORTING i_text         TYPE string
      RETURNING VALUE(rv_html) TYPE string.
    CLASS-METHODS normalize_markdown
      IMPORTING i_text         TYPE string
      RETURNING VALUE(rv_text) TYPE string.
    CLASS-METHODS code_block_to_html
      IMPORTING i_code         TYPE string
      RETURNING VALUE(rv_html) TYPE string.
    CLASS-METHODS source_block_to_html
      IMPORTING i_source       TYPE string
                i_title        TYPE string
      RETURNING VALUE(rv_html) TYPE string.
    CLASS-METHODS escape_html
      IMPORTING i_text         TYPE string
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS lcl_html IMPLEMENTATION.

  METHOD answer_to_html.
    DATA lv_text_upper TYPE string.

    lv_text_upper = i_answer.
    SHIFT lv_text_upper LEFT DELETING LEADING space.
    TRANSLATE lv_text_upper TO UPPER CASE.

    IF lv_text_upper CP '<!DOCTYPE HTML*'
    OR lv_text_upper CP '<!DOCTYPE*'
    OR lv_text_upper CP '<HTML*'.
      rv_html = i_answer.
    ELSE.
      DATA(lv_render_text) = render_abap_blocks( i_answer ).
      DATA(lv_source_html) = source_block_to_html(
        i_source = i_source
        i_title  = COND #( WHEN i_title IS NOT INITIAL THEN i_title ELSE 'Source code' ) ).

      rv_html = |<!doctype html><html><head><meta charset="utf-8">|
             && |<style>body\{font-family:"Segoe UI",Arial,sans-serif;font-size:14px;margin:0;|
             && |min-height:100vh;background:linear-gradient(135deg,#f8fbff 0%,#eef6ff 45%,#f7fff9 100%);|
             && |color:#1f2933;\}|
             && |.answer\{white-space:pre-wrap;font-family:"Segoe UI",Arial,sans-serif;line-height:1.45;|
             && |margin:14px;padding:16px 18px;background:rgba(255,255,255,.88);border:1px solid #dce8f6;|
             && |box-shadow:0 2px 10px rgba(56,96,140,.10);\}|
             && |.md_h\{display:block;font-size:17px;font-weight:700;color:#23476f;margin:4px 0 8px\}|
             && |.md_h2\{display:block;font-size:15px;font-weight:700;color:#23476f;margin:4px 0 6px\}|
             && |.md_li\{display:block;margin:2px 0 2px 18px;text-indent:-18px\}|
             && |code\{font-family:Consolas,monospace;background:#eef3f8;border:1px solid #d7e0ea;|
             && |padding:0 4px;color:#18324a\}|
             && |strong\{font-weight:700\}|
             && |.tokens\{display:inline-block;color:#005ea8;font-weight:700;background:#e8f3ff;|
             && |border:1px solid #b9dcff;padding:3px 7px;margin-top:6px;\}|
             && |.code_tbl\{border-collapse:collapse;width:100%;font:12px/1.5 Consolas,monospace;|
             && |background:#fff;border:1px solid #d7e0ea;margin:10px 0;\}|
             && |.source_title\{font-weight:700;color:#23476f;margin:14px 0 6px\}|
             && |.code_tbl tr:hover td\{background:#f0f4fa\}|
             && |.ln\{color:#aaa;text-align:right;padding:1px 10px 1px 5px;min-width:42px;|
             && |border-right:1px solid #e0e0e0;white-space:nowrap;background:#fafafa;user-select:none;\}|
             && |.cd\{padding:1px 8px;white-space:pre;\}|
             && |.cd-error\{padding:1px 8px;white-space:pre;color:red;font-weight:bold;\}|
             && |table.md\{border-collapse:collapse;margin:10px 0;\}|
             && |table.md th,table.md td\{border:1px solid #d7e0ea;padding:4px 8px;text-align:left;\}|
             && |table.md th\{background:#eef3f8;\}|
             && |</style></head><body><div class="answer">|
             && lv_render_text
             && lv_source_html
             && |</div></body></html>|.
    ENDIF.
  ENDMETHOD.

  METHOD render_abap_blocks.
    DATA lv_rest       TYPE string.
    DATA lv_before     TYPE string.
    DATA lv_code       TYPE string.
    DATA lv_after      TYPE string.
    DATA lv_start      TYPE i.
    DATA lv_end        TYPE i.
    DATA lv_code_start TYPE i.
    DATA lv_fence_len  TYPE i.

    lv_rest = i_text.

    DO.
      FIND FIRST OCCURRENCE OF REGEX '```\s*[A-Za-z0-9_-]*\s*' IN lv_rest
        MATCH OFFSET lv_start
        MATCH LENGTH lv_fence_len.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      lv_before     = substring( val = lv_rest len = lv_start ).
      lv_code_start = lv_start + lv_fence_len.
      lv_after      = substring( val = lv_rest off = lv_code_start ).
      FIND FIRST OCCURRENCE OF '```' IN lv_after MATCH OFFSET lv_end.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      lv_code = substring( val = lv_after len = lv_end ).
      SHIFT lv_code LEFT DELETING LEADING cl_abap_char_utilities=>newline.
      rv_text = rv_text
             && render_markdown_text( lv_before )
             && code_block_to_html( lv_code ).
      lv_rest = substring( val = lv_after off = lv_end + 3 ).
    ENDDO.

    rv_text = rv_text && render_markdown_text( lv_rest ).
  ENDMETHOD.

  METHOD render_markdown_text.
    DATA lt_lines   TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY.
    DATA lv_text    TYPE string.
    DATA lv_hashes  TYPE string.
    DATA lv_content TYPE string.
    DATA lv_marker  TYPE string.
    DATA lv_item    TYPE string.

    lv_text = normalize_markdown( i_text ).
    SPLIT lv_text AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO DATA(lv_line).
      DATA(lv_trimmed) = lv_line.
      SHIFT lv_trimmed LEFT DELETING LEADING space.

      IF lv_trimmed IS INITIAL.
        rv_html = rv_html && cl_abap_char_utilities=>newline.
        CONTINUE.
      ENDIF.

      FIND FIRST OCCURRENCE OF REGEX '^(#{1,6})\s+(.+)$' IN lv_trimmed
        SUBMATCHES lv_hashes lv_content.
      IF sy-subrc = 0.
        rv_html = rv_html
               && |<div class="{ COND #( WHEN strlen( lv_hashes ) <= 1 THEN 'md_h' ELSE 'md_h2' ) }">{ render_inline_markdown( lv_content ) }</div>|
               && cl_abap_char_utilities=>newline.
        CONTINUE.
      ENDIF.

      FIND FIRST OCCURRENCE OF REGEX '^([0-9]+\.)\s+(.+)$' IN lv_trimmed
        SUBMATCHES lv_marker lv_item.
      IF sy-subrc = 0.
        rv_html = rv_html
               && |<div class="md_li">{ escape_html( lv_marker ) } { render_inline_markdown( lv_item ) }</div>|
               && cl_abap_char_utilities=>newline.
        CONTINUE.
      ENDIF.

      FIND FIRST OCCURRENCE OF REGEX '^-\s+(.+)$' IN lv_trimmed
        SUBMATCHES lv_item.
      IF sy-subrc = 0.
        rv_html = rv_html
               && |<div class="md_li">- { render_inline_markdown( lv_item ) }</div>|
               && cl_abap_char_utilities=>newline.
        CONTINUE.
      ENDIF.

      FIND FIRST OCCURRENCE OF REGEX '^Tokens:' IN lv_trimmed.
      IF sy-subrc = 0.
        rv_html = rv_html
               && |<span class="tokens">{ render_inline_markdown( lv_trimmed ) }</span>|
               && cl_abap_char_utilities=>newline.
        CONTINUE.
      ENDIF.

      rv_html = rv_html
             && render_inline_markdown( lv_line )
             && cl_abap_char_utilities=>newline.
    ENDLOOP.
  ENDMETHOD.

  METHOD render_inline_markdown.
    rv_html = escape_html( i_text ).
    REPLACE ALL OCCURRENCES OF REGEX '\*\*([^*]+)\*\*' IN rv_html WITH '<strong>$1</strong>'.
    REPLACE ALL OCCURRENCES OF REGEX '`([^`]+)`' IN rv_html WITH '<code>$1</code>'.
  ENDMETHOD.

  METHOD normalize_markdown.
    rv_text = i_text.
    DATA(lv_nl) = cl_abap_char_utilities=>newline.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN rv_text WITH lv_nl.
    REPLACE ALL OCCURRENCES OF REGEX '\s+(#{1,6})\s+' IN rv_text WITH |{ lv_nl }{ lv_nl }$1 |.
    REPLACE ALL OCCURRENCES OF REGEX '\s+([0-9]+)\.\s+(\*\*)' IN rv_text WITH |{ lv_nl }$1. $2|.
    REPLACE ALL OCCURRENCES OF REGEX '\s+-\s+' IN rv_text WITH |{ lv_nl }- |.
  ENDMETHOD.

  METHOD code_block_to_html.
    DATA lt_lines TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY.
    DATA lv_lno   TYPE i.

    SPLIT i_code AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.
    rv_html = |<table class="code_tbl"><tbody>|.

    LOOP AT lt_lines INTO DATA(lv_line).
      lv_lno = lv_lno + 1.
      DATA(lv_class) = COND string(
        WHEN lv_line CS 'was not found or cannot be read'
          THEN 'cd-error'
          ELSE 'cd' ).
      rv_html = rv_html
             && |<tr><td class="ln">{ lv_lno }</td>|
             && |<td class="{ lv_class }">{ escape_html( i_text = lv_line ) }</td></tr>|.
    ENDLOOP.

    rv_html = rv_html && |</tbody></table>|.
  ENDMETHOD.

  METHOD source_block_to_html.
    IF i_source IS INITIAL.
      RETURN.
    ENDIF.
    rv_html = cl_abap_char_utilities=>newline
           && |<div class="source_title">{ escape_html( i_title ) }</div>|
           && code_block_to_html( i_source ).
  ENDMETHOD.

  METHOD escape_html.
    rv_text = i_text.
    REPLACE ALL OCCURRENCES OF '&' IN rv_text WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN rv_text WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN rv_text WITH '&gt;'.
    REPLACE ALL OCCURRENCES OF '"' IN rv_text WITH '&quot;'.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* lcl_popup - GUI popup with splitter: left=question, right=answer
*----------------------------------------------------------------------*
CLASS lcl_popup DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING i_base_url TYPE string
                i_model    TYPE text255
                i_apikey   TYPE string
                i_provider TYPE string
                i_question TYPE string OPTIONAL
                i_schema   TYPE string OPTIONAL.

    METHODS show.

  PRIVATE SECTION.
    DATA: mv_base_url TYPE string,
          mv_model    TYPE text255,
          mv_apikey   TYPE string,
          mv_provider TYPE string,
          mv_question TYPE string,
          mv_schema   TYPE string,
          mo_dialog   TYPE REF TO cl_gui_dialogbox_container,
          mo_toolbar  TYPE REF TO cl_gui_toolbar,
          mo_split    TYPE REF TO cl_gui_splitter_container,
          mo_question TYPE REF TO cl_gui_textedit,
          mo_schema   TYPE REF TO cl_gui_textedit,
          mo_answer   TYPE REF TO cl_gui_html_viewer.

    METHODS on_toolbar_click
      FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING fcode.

    METHODS on_dialog_close
      FOR EVENT close OF cl_gui_dialogbox_container.

    METHODS ask_ai.

    METHODS format_json
      IMPORTING i_json         TYPE string
      RETURNING VALUE(rv_json) TYPE string.
ENDCLASS.

CLASS lcl_popup IMPLEMENTATION.

  METHOD constructor.
    mv_base_url = i_base_url.
    mv_model    = i_model.
    mv_apikey   = i_apikey.
    mv_provider = i_provider.
    mv_question = i_question.
    mv_schema   = i_schema.
  ENDMETHOD.

  METHOD show.
    " Dialog popup container
    CREATE OBJECT mo_dialog
      EXPORTING
        caption  = 'Easy AI'
        top      = 20
        left     = 20
        width    = 1200
        height   = 620
        metric   = cl_gui_dialogbox_container=>metric_pixel
      EXCEPTIONS
        OTHERS   = 1.

    SET HANDLER on_dialog_close FOR mo_dialog.

    " Outer splitter: row1=toolbar, row2=editors
    DATA lo_outer TYPE REF TO cl_gui_splitter_container.
    CREATE OBJECT lo_outer
      EXPORTING
        parent  = mo_dialog
        rows    = 2
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    lo_outer->set_row_height( id = 1 height = 8 ).
    lo_outer->set_row_height( id = 2 height = 92 ).

    DATA lo_toolbar_cont TYPE REF TO cl_gui_container.
    lo_toolbar_cont = lo_outer->get_container( row = 1 column = 1 ).

    DATA lo_editors_cont TYPE REF TO cl_gui_container.
    lo_editors_cont = lo_outer->get_container( row = 2 column = 1 ).

    " Toolbar
    CREATE OBJECT mo_toolbar
      EXPORTING parent = lo_toolbar_cont
      EXCEPTIONS OTHERS = 1.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  TYPE cntl_simple_event.

    ls_event-eventid    = cl_gui_toolbar=>m_id_function_selected.
    ls_event-appl_event = space.
    APPEND ls_event TO lt_events.
    mo_toolbar->set_registered_events( events = lt_events ).

    DATA lt_buttons TYPE ttb_button.
    APPEND VALUE #( function  = 'ASK'
                    icon      = CONV #( icon_execute_object )
                    butn_type = cntb_btype_button
                    text      = 'Ask AI'
                    quickinfo = 'Send question to AI' ) TO lt_buttons.
    mo_toolbar->add_button_group( lt_buttons ).

    SET HANDLER on_toolbar_click FOR mo_toolbar.

    " Horizontal splitter: left=question, center=json schema, right=answer
    CREATE OBJECT mo_split
      EXPORTING
        parent  = lo_editors_cont
        rows    = 1
        columns = 3
      EXCEPTIONS
        OTHERS  = 1.

    mo_split->set_column_width( id = 1 width = 40 ).
    mo_split->set_column_width( id = 2 width = 25 ).
    mo_split->set_column_width( id = 3 width = 35 ).

    DATA lo_left   TYPE REF TO cl_gui_container.
    DATA lo_center TYPE REF TO cl_gui_container.
    DATA lo_right  TYPE REF TO cl_gui_container.
    lo_left   = mo_split->get_container( row = 1 column = 1 ).
    lo_center = mo_split->get_container( row = 1 column = 2 ).
    lo_right  = mo_split->get_container( row = 1 column = 3 ).

    " Question editor (left)
    CREATE OBJECT mo_question
      EXPORTING parent = lo_left
      EXCEPTIONS OTHERS = 1.
    mo_question->set_toolbar_mode( 0 ).

    " JSON schema editor (center)
    CREATE OBJECT mo_schema
      EXPORTING parent = lo_center
      EXCEPTIONS OTHERS = 1.
    mo_schema->set_toolbar_mode( 0 ).

    " Answer viewer (right): HTML viewer to render the markdown answer
    CREATE OBJECT mo_answer
      EXPORTING parent = lo_right
      EXCEPTIONS OTHERS = 1.

    " Pre-fill question and schema from file if provided
    IF mv_question IS NOT INITIAL.
      DATA lt_qlines TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
      SPLIT mv_question AT cl_abap_char_utilities=>newline INTO TABLE lt_qlines.
      mo_question->set_text_as_stream( text = lt_qlines ).
    ENDIF.
    IF mv_schema IS NOT INITIAL.
      DATA lt_slines TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
      SPLIT mv_schema AT cl_abap_char_utilities=>newline INTO TABLE lt_slines.
      mo_schema->set_text_as_stream( text = lt_slines ).
    ENDIF.

    CALL METHOD cl_gui_cfw=>flush.
  ENDMETHOD.

  METHOD on_dialog_close.
    mo_dialog->free( ).
    CLEAR mo_dialog.
    CALL METHOD cl_gui_cfw=>flush.
  ENDMETHOD.

  METHOD on_toolbar_click.
    CHECK fcode = 'ASK'.
    ask_ai( ).
  ENDMETHOD.

  METHOD ask_ai.
    " cl_gui_textedit works with flat charlike tables
    TYPES: ty_line(255) TYPE c,
           ty_lines     TYPE TABLE OF ty_line.

    DATA lt_lines TYPE ty_lines.
    mo_question->get_text_as_stream( IMPORTING text = lt_lines ).

    DATA lv_prompt TYPE string.
    LOOP AT lt_lines INTO DATA(ls_line).
      IF lv_prompt IS NOT INITIAL.
        lv_prompt = lv_prompt && cl_abap_char_utilities=>newline.
      ENDIF.
      lv_prompt = lv_prompt && ls_line.
    ENDLOOP.
    CONDENSE lv_prompt.

    IF lv_prompt IS INITIAL.
      MESSAGE 'Please enter a question' TYPE 'I'.
      RETURN.
    ENDIF.

    " Read JSON schema from center panel (strip trailing spaces from each char-255 line)
    DATA lt_schema_lines TYPE ty_lines.
    mo_schema->get_text_as_stream( IMPORTING text = lt_schema_lines ).
    DATA lv_json_schema TYPE string.
    LOOP AT lt_schema_lines INTO DATA(ls_schema_line).
      DATA lv_schema_str TYPE string.
      DATA lv_slen       TYPE i.
      lv_schema_str = ls_schema_line.
      " Remove trailing spaces from char-255 field padding
      lv_slen = strlen( lv_schema_str ).
      WHILE lv_slen > 0.
        lv_slen = lv_slen - 1.
        IF lv_schema_str+lv_slen(1) <> ` `.
          lv_slen = lv_slen + 1.
          EXIT.
        ENDIF.
      ENDWHILE.
      IF lv_slen > 0.
        lv_json_schema = lv_json_schema && lv_schema_str+0(lv_slen).
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING percentage = 50 text = 'Asking AI...'.

    DATA(lv_answer) = lcl_ai_api=>ask(
      i_prompt      = lv_prompt
      i_base_url    = mv_base_url
      i_model       = mv_model
      i_apikey      = mv_apikey
      i_provider    = mv_provider
      i_json_schema = lv_json_schema ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING percentage = 0 text = ''.

    " Replace literal \n \r \t \" escape sequences with real characters so the
    " markdown renderer sees real line breaks.
    REPLACE ALL OCCURRENCES OF '\n' IN lv_answer WITH cl_abap_char_utilities=>newline.
    REPLACE ALL OCCURRENCES OF '\r' IN lv_answer WITH ''.
    REPLACE ALL OCCURRENCES OF '\t' IN lv_answer WITH cl_abap_char_utilities=>horizontal_tab.
    REPLACE ALL OCCURRENCES OF '\"' IN lv_answer WITH '"'.

    " Render markdown -> HTML and load it into the answer viewer.
    DATA(lv_html) = lcl_html=>answer_to_html( i_answer = lv_answer ).

    DATA lt_html   TYPE STANDARD TABLE OF w3html WITH DEFAULT KEY.
    DATA ls_html   TYPE w3html.
    DATA lv_offset TYPE i.
    WHILE lv_offset < strlen( lv_html ).
      CLEAR ls_html.
      ls_html-line = substring(
        val = lv_html
        off = lv_offset
        len = nmin( val1 = 255 val2 = strlen( lv_html ) - lv_offset ) ).
      APPEND ls_html TO lt_html.
      lv_offset = lv_offset + 255.
    ENDWHILE.

    DATA lv_url TYPE c LENGTH 255.
    mo_answer->load_data(
      EXPORTING  type         = 'text'
                 subtype      = 'html'
      IMPORTING  assigned_url = lv_url
      CHANGING   data_table   = lt_html
      EXCEPTIONS OTHERS       = 1 ).

    mo_answer->show_url(
      EXPORTING url = lv_url
      EXCEPTIONS OTHERS = 1 ).

    CALL METHOD cl_gui_cfw=>flush.
  ENDMETHOD.

  METHOD format_json.
    " Try to detect and pretty-print JSON in the response
    DATA lv_trimmed TYPE string.
    lv_trimmed = i_json.
    CONDENSE lv_trimmed.

    " Check if it looks like JSON object or array
    DATA lv_first TYPE c LENGTH 1.
    lv_first = lv_trimmed(1).
    IF lv_first <> '{' AND lv_first <> '['.
      rv_json = i_json.
      RETURN.
    ENDIF.

    " Walk through characters and add indentation
    DATA: lv_indent   TYPE i VALUE 0,
          lv_in_str   TYPE abap_bool VALUE abap_false,
          lv_escaped  TYPE abap_bool VALUE abap_false,
          lv_len      TYPE i,
          lv_char     TYPE c LENGTH 1,
          lv_prev     TYPE c LENGTH 1,
          lv_nl       TYPE string,
          lv_result   TYPE string.

    lv_nl   = cl_abap_char_utilities=>newline.
    lv_len  = strlen( lv_trimmed ).

    DO lv_len TIMES.
      DATA lv_pos TYPE i.
      lv_pos  = sy-index - 1.
      lv_char = lv_trimmed+lv_pos(1).

      " Handle escape sequences inside strings
      IF lv_escaped = abap_true.
        lv_result  = lv_result && lv_char.
        lv_escaped = abap_false.
        lv_prev    = lv_char.
        CONTINUE.
      ENDIF.

      IF lv_char = '\' AND lv_in_str = abap_true.
        lv_result  = lv_result && lv_char.
        lv_escaped = abap_true.
        lv_prev    = lv_char.
        CONTINUE.
      ENDIF.

      " Toggle string mode on unescaped quote
      IF lv_char = '"'.
        lv_in_str = COND abap_bool( WHEN lv_in_str = abap_false THEN abap_true ELSE abap_false ).
        lv_result = lv_result && lv_char.
        lv_prev   = lv_char.
        CONTINUE.
      ENDIF.

      " Outside strings: format structural characters
      IF lv_in_str = abap_false.
        CASE lv_char.
          WHEN '{' OR '['.
            lv_indent  = lv_indent + 2.
            lv_result  = lv_result && lv_char && lv_nl && repeat( val = ` ` occ = lv_indent ).
          WHEN '}' OR ']'.
            lv_indent  = lv_indent - 2.
            IF lv_indent < 0. lv_indent = 0. ENDIF.
            lv_result  = lv_result && lv_nl && repeat( val = ` ` occ = lv_indent ) && lv_char.
          WHEN ','.
            lv_result  = lv_result && lv_char && lv_nl && repeat( val = ` ` occ = lv_indent ).
          WHEN ':'.
            lv_result  = lv_result && lv_char && ` `.
          WHEN OTHERS.
            " Skip whitespace/newlines that were already in the input
            IF lv_char <> ` ` AND lv_char <> cl_abap_char_utilities=>newline AND
               lv_char <> cl_abap_char_utilities=>cr_lf(1).
              lv_result = lv_result && lv_char.
            ENDIF.
        ENDCASE.
      ELSE.
        lv_result = lv_result && lv_char.
      ENDIF.

      lv_prev = lv_char.
    ENDDO.

    rv_json = lv_result.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Global variables
*----------------------------------------------------------------------*
DATA go_popup TYPE REF TO lcl_popup.

*----------------------------------------------------------------------*
* INITIALIZATION - fill the provider listbox, suppress F8 (ONLI)
*----------------------------------------------------------------------*
INITIALIZATION.
  DATA lt_excl TYPE TABLE OF sy-ucomm.
  APPEND 'ONLI' TO lt_excl.
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING  p_status  = sy-pfkey
    TABLES     p_exclude = lt_excl.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT - fill the model listbox live from the API
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  " Provider listbox values must be (re)set on every PBO, otherwise they are
  " lost after a round-trip and the selection does not stick.
  DATA lt_prov TYPE vrm_values.
  lt_prov = VALUE #(
    ( key = 'ANTHROPIC' text = 'https://api.anthropic.com' )
    ( key = 'OPENAI'    text = 'https://api.openai.com' )
    ( key = 'MISTRAL'   text = 'https://api.mistral.ai' ) ).
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING id     = 'P_PROV'
              values = lt_prov.

  " Re-fetch the model list only when provider or key changed; otherwise reuse
  " the cached list. Re-fetch is skipped on plain Enter (state unchanged).
  DATA(lv_state) = |{ p_prov }\|{ p_apikey }|.
  IF p_apikey IS NOT INITIAL AND gv_loaded_key <> lv_state.

    DATA: lt_ids TYPE lcl_ai_api=>tt_ids,
          lv_err TYPE string.
    lcl_ai_api=>list_models(
      EXPORTING i_base_url = lcl_ai_api=>base_url( p_prov )
                i_apikey   = CONV string( p_apikey )
                i_provider = lcl_ai_api=>provider_of( p_prov )
      IMPORTING et_ids     = lt_ids
                e_error    = lv_err ).

    CLEAR gt_model_vrm.
    LOOP AT lt_ids INTO DATA(lv_id).
      APPEND VALUE #( key = lv_id text = lv_id ) TO gt_model_vrm.
    ENDLOOP.

    IF lt_ids IS NOT INITIAL.
      " Default to the first model when the current one is not in the new list.
      " CONV string trims the trailing blanks of the char-255 parameter so the
      " comparison against the (string) ids actually matches.
      DATA(lv_cur_model) = CONV string( p_model ).
      IF lv_cur_model IS INITIAL OR NOT line_exists( lt_ids[ table_line = lv_cur_model ] ).
        p_model = lt_ids[ 1 ].
      ENDIF.
      " Remember the loaded state only on success, so a failed call (e.g. a key
      " that does not match the chosen provider) is retried on the next refresh.
      gv_loaded_key = lv_state.
    ELSE.
      " Fetch failed / empty: clear the stale model so the listbox does not keep
      " showing the previous provider's value as a single leftover entry.
      CLEAR p_model.
    ENDIF.
  ENDIF.

  " Model listbox values, like the provider list, must be set on every PBO.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING id     = 'P_MODEL'
              values = gt_model_vrm.

  " File listbox: rescan folder when path changes
  DATA(lv_folder) = CONV string( p_folder ).
  CONDENSE lv_folder.
  IF lv_folder IS NOT INITIAL AND gv_loaded_folder <> lv_folder.
    CLEAR gt_file_vrm.
    DATA lt_md_files TYPE filetable.
    DATA lv_md_cnt   TYPE i.
    cl_gui_frontend_services=>directory_list_files(
      EXPORTING directory  = lv_folder
                filter     = '*.md'
      CHANGING  file_table = lt_md_files
                count      = lv_md_cnt
      EXCEPTIONS OTHERS = 1 ).
    SORT lt_md_files BY filename.
    LOOP AT lt_md_files INTO DATA(ls_md_file).
      APPEND VALUE #( key = ls_md_file-filename text = ls_md_file-filename ) TO gt_file_vrm.
    ENDLOOP.
    IF gt_file_vrm IS NOT INITIAL.
      p_file = gt_file_vrm[ 1 ]-key.
    ELSE.
      CLEAR p_file.
    ENDIF.
    gv_loaded_folder = lv_folder.
  ENDIF.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING id     = 'P_FILE'
              values = gt_file_vrm.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN - open popup on Enter (once key + model are set)
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CHECK sy-ucomm IS INITIAL OR sy-ucomm = 'UCCHECK'.

  IF p_apikey IS INITIAL OR p_model IS INITIAL.
    RETURN.
  ENDIF.

  " Read selected .md and optional matching .json from presentation server
  DATA lv_question TYPE string.
  DATA lv_schema   TYPE string.
  IF p_file IS NOT INITIAL AND p_folder IS NOT INITIAL.
    DATA(lv_md_path) = CONV string( p_folder ).
    CONDENSE lv_md_path.
    DATA(lv_last_idx) = strlen( lv_md_path ) - 1.
    IF lv_last_idx >= 0.
      DATA(lv_last_char) = lv_md_path+lv_last_idx(1).
      IF lv_last_char <> '\' AND lv_last_char <> '/'.
        lv_md_path = lv_md_path && '\'.
      ENDIF.
    ENDIF.
    lv_md_path = lv_md_path && CONV string( p_file ).

    DATA lt_upload TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    cl_gui_frontend_services=>gui_upload(
      EXPORTING filename = lv_md_path
                filetype = 'ASC'
      CHANGING  data_tab = lt_upload
      EXCEPTIONS OTHERS  = 1 ).
    LOOP AT lt_upload INTO DATA(lv_ul_line).
      IF lv_question IS NOT INITIAL.
        lv_question = lv_question && cl_abap_char_utilities=>newline.
      ENDIF.
      lv_question = lv_question && lv_ul_line.
    ENDLOOP.

    " Try to read matching .json (same base name)
    DATA(lv_json_path) = lv_md_path.
    REPLACE REGEX '\.md$' IN lv_json_path WITH '.json'.
    IF lv_json_path <> lv_md_path.
      CLEAR lt_upload.
      cl_gui_frontend_services=>gui_upload(
        EXPORTING filename = lv_json_path
                  filetype = 'ASC'
        CHANGING  data_tab = lt_upload
        EXCEPTIONS OTHERS  = 1 ).
      IF sy-subrc = 0.
        LOOP AT lt_upload INTO lv_ul_line.
          lv_schema = lv_schema && lv_ul_line.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

  go_popup = NEW lcl_popup(
    i_base_url = lcl_ai_api=>base_url( p_prov )
    i_model    = p_model
    i_apikey   = CONV string( p_apikey )
    i_provider = lcl_ai_api=>provider_of( p_prov )
    i_question = lv_question
    i_schema   = lv_schema ).

  go_popup->show( ).

*----------------------------------------------------------------------*
* START-OF-SELECTION - never reached (F8 suppressed)
*----------------------------------------------------------------------*
START-OF-SELECTION.
