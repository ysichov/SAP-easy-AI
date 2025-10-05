*&---------------------------------------------------------------------*
*& Report Z_EASY_AI
*&---------------------------------------------------------------------*
*&Easy and small AI API example
*&---------------------------------------------------------------------*
REPORT z_easy_ai.

PARAMETERS: p_prompt(200),
            p_dest        TYPE text255 MEMORY ID dest,
            p_model       TYPE text255 MEMORY ID model,
            p_apikey      TYPE text255  MEMORY ID api.

CLASS lcl_ai_api DEFINITION.

  PUBLIC SECTION.

    METHODS  call_openai  IMPORTING i_prompt TYPE string .

  PRIVATE SECTION.
    DATA: m_api_key TYPE string.

    METHODS:
      build_request
        IMPORTING
          i_prompt  TYPE string
        EXPORTING
          e_payload TYPE string,

      send_request
        IMPORTING
          i_payload  TYPE string
        EXPORTING
          e_response TYPE string
          e_error    TYPE xfeld,

      output
        IMPORTING
          i_prompt  TYPE string
          i_content TYPE string.
ENDCLASS.

CLASS lcl_ai_api IMPLEMENTATION.

  METHOD call_openai.
    DATA: prompt   TYPE string,
          payload  TYPE string,
          response TYPE string.

    "Build payload
    CALL METHOD build_request
      EXPORTING
        i_prompt  = i_prompt
      IMPORTING
        e_payload = payload.

    CALL METHOD me->send_request
      EXPORTING
        i_payload  = payload
      IMPORTING
        e_response = response
        e_error    = DATA(error).

    IF error IS NOT INITIAL.
      cl_demo_output=>display(
     | PROMPT: { i_prompt } { cl_abap_char_utilities=>cr_lf  } CONTENT: { response } { cl_abap_char_utilities=>cr_lf } | ).

    ELSE.
      CALL METHOD me->output
        EXPORTING
          i_prompt  = i_prompt
          i_content = response.

    ENDIF.

  ENDMETHOD.

  METHOD build_request.

    DATA: payload TYPE string.
    payload = |{ '{ "model": "' && p_model && '", "messages": [{ "role": "user", "content": "' && i_prompt &&  '" }], "max_tokens": 1000 } ' }|.
    e_payload = payload.

  ENDMETHOD.

  METHOD send_request.

    DATA: response_body TYPE string,
          header        TYPE string.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = p_dest
      IMPORTING
        client                   = data(o_http_client)
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 13.

    IF sy-subrc = 2.
      e_response = 'Destination not found. Please check it in SM59 transaction'.
      e_error = abap_true.
      RETURN.
    ELSEIF sy-subrc <> 0.
      e_response = |cl_http_client=>create_by_destination error №' { sy-subrc }|.
      e_error = abap_true.
      RETURN.
    ENDIF.

    m_api_key = p_apikey. "any name for local LLMs
    "set request header
    o_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
    o_http_client->request->set_header_field( name = 'Authorization' value = |Bearer { m_api_key }| ).

    o_http_client->request->set_method('POST').

    "set payload
    o_http_client->request->set_cdata( i_payload ).

    CALL METHOD o_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.

    IF sy-subrc = 0.
      CALL METHOD o_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.
      "Get response
      IF sy-subrc <> 0.
        response_body = o_http_client->response->get_data( ).
        e_response = response_body.
      ELSE.
        response_body = o_http_client->response->get_data( ).
        IF response_body IS NOT INITIAL.
          e_response = response_body.
        ELSE.
          e_response = 'Call was succeesful, but got no response'.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD output.

    DATA: text(1000) TYPE c,
          str        TYPE string,
          content    TYPE string,
          reasoning  TYPE string.

    TYPES: BEGIN OF t_message,
             role              TYPE string,
             content           TYPE string,
             reasoning_content TYPE string,
           END           OF t_message,

           t_t_message TYPE STANDARD TABLE OF t_message WITH NON-UNIQUE DEFAULT KEY,

           BEGIN OF t_choice,
             index         TYPE string,
             message       TYPE t_message,
             logprobs      TYPE string,
             finish_reason TYPE string,
           END      OF t_choice,

           BEGIN OF t_base_openai_res,
             id      TYPE string,
             object  TYPE string,
             created TYPE string,
             model   TYPE string,
             choices TYPE TABLE OF t_choice WITH NON-UNIQUE DEFAULT KEY,
           END OF t_base_openai_res.

    DATA: response TYPE t_base_openai_res,
          binary      TYPE xstring,
          o_x2c       TYPE REF TO cl_abap_conv_in_ce.

    o_x2c = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    binary = i_content.
    o_x2c->convert( EXPORTING input = binary
                     IMPORTING data  = str ).

    /ui2/cl_json=>deserialize( EXPORTING json = str CHANGING data = response ).

    IF  response-choices IS NOT INITIAL.
      content = response-choices[ 1 ]-message-content.
      reasoning = response-choices[ 1 ]-message-reasoning_content.
    ELSE.
      content = str.
      cl_abap_browser=>show_html(  html_string = content title = 'Error (' ).
      RETURN.
    ENDIF.

    cl_demo_output=>display(
     | PROMPT: { i_prompt } { cl_abap_char_utilities=>cr_lf  } CONTENT: { content } { cl_abap_char_utilities=>cr_lf } reasoning: { reasoning } | ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  NEW lcl_ai_api( )->call_openai( CONV #( p_prompt ) ).
