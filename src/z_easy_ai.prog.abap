*&---------------------------------------------------------------------*
*& Report Z_EASY_AI
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_easy_ai.

PARAMETERS: p_prompt(200).

CLASS lCL_AI_API DEFINITION.
  PUBLIC SECTION.

    METHODS  call_openai  IMPORTING iv_prompt TYPE string .
  PRIVATE SECTION.
    DATA: mv_api_key TYPE string.

    METHODS: build_request
      IMPORTING
        iv_prompt  TYPE string
      EXPORTING
        ev_payload TYPE string ,
      send_request
        IMPORTING
          iv_payload  TYPE string
        EXPORTING
          ev_response TYPE string,
      output
        IMPORTING
          iv_prompt  TYPE string
          iv_content TYPE string .
ENDCLASS.

CLASS lCL_AI_API IMPLEMENTATION.

  METHOD call_openai.
    DATA: lv_prompt   TYPE string,
          lv_payload  TYPE string,
          lv_response TYPE string.

    "Build payload
    CALL METHOD build_request
      EXPORTING
        iv_prompt  = iv_prompt
      IMPORTING
        ev_payload = lv_payload.

    CALL METHOD me->send_request
      EXPORTING
        iv_payload  = lv_payload
      IMPORTING
        ev_response = lv_response.

    CALL METHOD me->output
      EXPORTING
        iv_prompt  = iv_prompt
        iv_content = lv_response.

  ENDMETHOD.

  METHOD build_request.

    DATA: lv_payload TYPE string.

    lv_payload = |{ '{ "model": "any_name", "messages": [{ "role": "user", "content": "' && iv_prompt &&  '" }], "max_tokens": 1000 } ' }|.

    ev_payload = lv_payload.
  ENDMETHOD.

  METHOD send_request.

    DATA: lo_http_client   TYPE REF TO if_http_client,
          lv_response_body TYPE string,
          lv_header        TYPE string.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination                = 'Z_LM'
      IMPORTING
        client                     = lo_http_client
      EXCEPTIONS
        argument_not_found         = 1
        destination_not_found      = 2
        destination_no_authority   = 3
        plugin_not_active          = 4
        internal_error             = 5
        oa2c_set_token_error       = 6
        oa2c_missing_authorization = 7
        oa2c_invalid_config        = 8
        oa2c_invalid_parameters    = 9
        oa2c_invalid_scope         = 10
        oa2c_invalid_grant         = 11
        oa2c_secstore_adm          = 12
        OTHERS                     = 13.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

    mv_api_key = 'lmstudio'. "any name for local LLMs
    "set request header
    lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
    lo_http_client->request->set_header_field( name = 'Authorization' value = |Bearer { mv_api_key }| ).

    lo_http_client->request->set_method('POST').

    "set payload
    lo_http_client->request->set_cdata( iv_payload ).

    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.
    IF sy-subrc = 0.
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.
      "Get response
      IF sy-subrc <> 0.
        lv_response_body = lo_http_client->response->get_data( ).
        ev_response = lv_response_body.
      ELSE.
        lv_response_body = lo_http_client->response->get_data( ).
        IF lv_response_body IS NOT INITIAL.
          ev_response = lv_response_body.
        ELSE.
          ev_response = 'Call was succeesful, but got no response'.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD output.
    DATA: lv_text(1000) TYPE c,
          lv_string     TYPE string,
          lv_content    TYPE string,
          lv_reasoning  TYPE string.

    TYPES: BEGIN OF lty_s_message,
             role              TYPE string,
             content           TYPE string,
             reasoning_content TYPE string,
           END           OF lty_s_MESSAGE,
           lty_t_message TYPE STANDARD TABLE OF lty_s_message WITH NON-UNIQUE DEFAULT KEY,
           BEGIN OF lty_s_choice,
             index         TYPE string,
             message       TYPE lty_s_message,
             logprobs      TYPE string,
             finish_reason TYPE string,
           END      OF lty_s_choice,
           BEGIN OF lty_s_base_chatgpt_res,
             id      TYPE string,
             object  TYPE string,
             created TYPE string,
             model   TYPE string,
             choices TYPE TABLE OF lty_s_choice WITH NON-UNIQUE DEFAULT KEY,
           END OF lty_s_base_chatgpt_res.

    DATA ls_response TYPE lty_s_base_chatgpt_res.

    DATA: lv_binary TYPE xstring.

    DATA: lo_x2c TYPE REF TO cl_abap_conv_in_ce.
    lo_x2c = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    lv_binary = iv_content.
    lo_x2c->convert( EXPORTING input = lv_binary
                     IMPORTING data  = lv_string ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_string CHANGING data = ls_response ).


    IF  ls_response-choices IS NOT INITIAL.
      lv_content = ls_response-choices[ 1 ]-message-content.
      lv_reasoning = ls_response-choices[ 1 ]-message-reasoning_content.
    ELSE.
      lv_content = lv_string.
    ENDIF.

    cl_demo_output=>display(
     | PROMPT: { iv_prompt } { cl_abap_char_utilities=>cr_lf  } CONTENT: { lv_content } { cl_abap_char_utilities=>cr_lf } reasoning: { lv_reasoning } | ).
  ENDMETHOD.


ENDCLASS.


START-OF-SELECTION.

  DATA(lo_AI) = NEW zcl_ai_api( ).

  lo_ai->call_openai( CONV #( p_prompt ) ).
