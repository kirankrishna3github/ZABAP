class ZCL_TRUECOPY_DS_API definition
  public
  final
  create public .

public section.

  types:
    begin of mty_ds_parameters,
        sign_loc_p  type string,  " page no. on which the DS should be placed
        sign_loc_x  type string,  " no. of columns to the right from bottom left corner
        sign_loc_y  type string,  " no. of rows above the bottom left corner
        approved_by type string,
      end of mty_ds_parameters .
  types:
    begin of mty_ds_parameters_int,
        sign_loc    type string,  " sign location in format sign_loc_p[sign_loc_x:sign_loc_y]
        approved_by type string,
      end of mty_ds_parameters_int .
  types MTTY_MESSAGE type STRING_TABLE .

  constants:
    begin of mc_api_type,
        multipart type c length 1 value '0',
        base64    type c length 1 value '1',
      end of mc_api_type .
  constants:
    begin of mc_status_code,
        ok                    type string value '200',
        internal_server_error type string value '500',
      end of mc_status_code .
  constants:
    begin of mc_api_endpoint_url,
        multipart type string value 'https://indofil.truecopy.in:443/ws/v1/signpdf',
        base64    type string value 'https://indofil.truecopy.in:443/ws/v1/signstructdataRetB64',
        status    type string value 'https://indofil.truecopy.in:443/ws/v1/status',
      end of mc_api_endpoint_url .
  constants MC_FILE_FILTER_PDF type STRING value 'PDF files (*.pdf)|*.pdf' ##NO_TEXT.

  methods SIGN
    importing
      value(IS_DS_PARAMETERS) type MTY_DS_PARAMETERS
      value(IV_PDF_BINARY_DATA) type XSTRING optional
      value(IT_SMARTF_OTF_DATA) type TSFOTF optional
      value(IV_API_TYPE) type CHAR1 default MC_API_TYPE-MULTIPART
      value(IV_DISPLAY) type ABAP_BOOL default ABAP_TRUE
    exporting
      value(ET_MESSAGE) type MTTY_MESSAGE
    returning
      value(RV_SIGNED_PDF_BINARY_DATA) type XSTRING .
  methods CONSTRUCTOR .
  class-methods GET_NUM_OF_PDF_PAGES
    importing
      value(IV_PDF_BINARY_DATA) type XSTRING optional
      value(IT_SMARTF_OTF_DATA) type TSFOTF optional
    returning
      value(RV_NUM_OF_PAGES) type I .
  protected section.
  private section.

    data mv_running type abap_bool .
    data mv_header_ts type string .
    data mv_timestamp type string .
    data mv_pfxid type string .
    data mv_pfxpwd type string .
    data mv_apikey type string .
    data mv_checksum type string .
    data mt_message type string_table .

    methods get_messages
      returning
        value(rt_message) type mtty_message .
    methods sign_multipart
      importing
        value(is_ds_parameters)          type mty_ds_parameters_int
        value(iv_pdf_binary_data)        type xstring
      returning
        value(rv_signed_pdf_binary_data) type xstring .
    methods sign_base64
      importing
        value(is_ds_parameters)          type mty_ds_parameters_int
        value(iv_pdf_binary_data)        type xstring
      exporting
        value(et_message)                type mtty_message
      returning
        value(rv_signed_pdf_binary_data) type xstring .
    methods get_timestamp
      returning
        value(rv_timestamp) type string .
    methods get_pfxid
      returning
        value(rv_pfxid) type string .
    methods get_pfxpwd
      returning
        value(rv_pfxpwd) type string .
    methods get_apikey
      returning
        value(rv_apikey) type string .
    methods get_checksum
      returning
        value(rv_checksum) type string .
    methods check_ds_server_status
      returning
        value(rv_running) type abap_bool .
    methods add_message
      importing
        value(iv_text)       type string optional
        value(is_symsg)      type symsg optional
        value(iox_exception) type ref to cx_root optional .
    methods create_rest_client
      importing
        value(iv_api_endpoint_url) type string
      returning
        value(ro_rest_client)      type ref to cl_rest_http_client .
    methods cleanup .
    methods initialize .
ENDCLASS.



CLASS ZCL_TRUECOPY_DS_API IMPLEMENTATION.


  method add_message.
    try.
        data(lt_abap_callstack) = cl_abap_get_call_stack=>format_call_stack_with_struct(
                                    exporting
                                      stack = cl_abap_get_call_stack=>get_call_stack( ) ).

        split lt_abap_callstack[ 2 ]-event at '=>' into data(lv_class_name) data(lv_method_name).

        if iv_text is not initial.
          data(lv_text) = iv_text.
          append conv #( |{ lv_method_name } - { lv_text }| ) to mt_message.
        endif.

        if is_symsg is not initial.
          data(ls_symsg) = is_symsg.
          ls_symsg-msgid = cond #( when is_symsg-msgid is not initial
                                   then is_symsg-msgid
                                   else 'Z_DS' ).

          ls_symsg-msgty = 'E'.

          clear lv_text.
          message id ls_symsg-msgid type ls_symsg-msgty number ls_symsg-msgno
            with ls_symsg-msgv1 ls_symsg-msgv2 ls_symsg-msgv3 ls_symsg-msgv4
            into lv_text.

          append conv #( |{ lv_method_name } - { lv_text }| ) to mt_message.
        endif.

        if iox_exception is bound.
          clear lv_text.
          lv_text = iox_exception->get_text( ).

          append conv #( |{ lv_method_name } - { lv_text }| ) to mt_message.
        endif.
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.


  method check_ds_server_status.
    try.
        clear rv_running.

        data(lo_rest_client) = create_rest_client(
                                 exporting
                                   iv_api_endpoint_url = mc_api_endpoint_url-status ).

        if lo_rest_client is bound.
          try.
              lo_rest_client->if_rest_client~get( ).
            catch cx_rest_client_exception into data(lox_rest_client).
              add_message( exporting iv_text = conv #( lox_rest_client->get_text( ) ) ).
          endtry.

          data(lo_response) = lo_rest_client->if_rest_client~get_response_entity( ).

          if lo_response is bound.
            data(lv_status_code) = lo_response->get_header_field(
                                     exporting
                                       iv_name = if_http_header_fields_sap=>status_code ).

            case lv_status_code.
              when mc_status_code-ok.
                data:
                  begin of ls_server_status,
                    message type string,
                    status  type string,
                    errcode type string,
                  end of ls_server_status.

                data(lv_response_string) = lo_response->get_string_data( ).

                clear ls_server_status.
                /ui2/cl_json=>deserialize(
                  exporting
                    json             = lv_response_string                       " JSON string
                    pretty_name      = /ui2/cl_json=>pretty_mode-low_case       " Pretty Print property names
                  changing
                    data             = ls_server_status ).                      " Data to serialize

                rv_running = boolc( ls_server_status-status = '0' ).
              when mc_status_code-internal_server_error.
                rv_running = abap_false.
              when others.
            endcase.

            data(lv_response_ts) = lo_response->get_header_field(
                                     exporting
                                       iv_name = if_http_header_fields=>date ).

            if lv_response_ts is not initial.
              lv_response_ts = condense( lv_response_ts ).
              mv_header_ts = lv_response_ts.
            endif.
          endif.

          " close the rest client - also closes the http client
          lo_rest_client->if_rest_client~close( ).
        endif.

        mv_running = rv_running.
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.


  method cleanup.
    clear:
      mv_running,
      mv_header_ts,
      mv_timestamp,
      mv_pfxid,
      mv_pfxpwd,
      mv_apikey,
      mv_checksum,
      mt_message.
  endmethod.


  method constructor.
    try.
        cleanup( ).
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.


  method create_rest_client.
    clear ro_rest_client.

    cl_http_client=>create_by_url(
      exporting
        url                = iv_api_endpoint_url         " URL
      importing
        client             = data(lo_http_client)        " HTTP Client Abstraction
      exceptions
        argument_not_found = 1             " Communication Parameters (Host or Service) Not Available
        plugin_not_active  = 2             " HTTP/HTTPS Communication Not Available
        internal_error     = 3             " Internal Error (e.g. name too long)
        others             = 4 ).
    if sy-subrc <> 0.
      add_message( exporting is_symsg = corresponding #( sy ) ).
    endif.

    if lo_http_client is bound.
      " get the rest client using http client for access to easier/pre-written/reusable rest method calls
      ro_rest_client = new cl_rest_http_client( io_http_client = lo_http_client ).
    endif.
  endmethod.


  method get_apikey.
    try.
        clear rv_apikey.
        rv_apikey = mv_apikey = cond #( when zcl_helper=>is_development( )
                                          or zcl_helper=>is_quality( )
                                          or zcl_helper=>is_sandbox( )
                                        then 'BPUCZXPG'
                                        else 'prd_apikey'  ).
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.


  method get_checksum.
    try.
        clear rv_checksum.
        rv_checksum = mv_checksum = substring( val = cl_nwbc_utility=>to_md5(
                                                       exporting
                                                         iv_value = |{ mv_apikey }| &&
                                                                    |{ mv_timestamp }| ) off = 0 len = 16 ).
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.


  method get_messages.
    try.
        clear rt_message.

        sort mt_message ascending as text.
        delete adjacent duplicates from mt_message comparing all fields.

        rt_message = mt_message.
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.


  method get_num_of_pdf_pages.
    clear rv_num_of_pages.

    data(lv_string) = cl_bcs_convert=>xstring_to_string(
                        exporting
                          iv_xstr = iv_pdf_binary_data
                          iv_cp   = '4110' ).

    find all occurrences of '/Count' in lv_string results data(lt_count).

    rv_num_of_pages = reduce #( init pages = 0   " to calculate the number of pages in the smartform
                                for ls in it_smartf_otf_data
                                  where ( tdprintcom = 'EP' )
                                next pages = pages + 1 ).
  endmethod.


  method get_pfxid.
    try.
        clear rv_pfxid.
        rv_pfxid = mv_pfxid =  cond #( when zcl_helper=>is_development( )
                                         or zcl_helper=>is_quality( )
                                         or zcl_helper=>is_sandbox( )
                                       then 'samco_indofil'
                                       else 'prd_pfxid' ).
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.


  method get_pfxpwd.
    try.
        clear rv_pfxpwd.
        rv_pfxpwd = mv_pfxpwd = cond #( when zcl_helper=>is_development( )
                                          or zcl_helper=>is_quality( )
                                          or zcl_helper=>is_sandbox( )
                                        then 'samco'
                                        else 'prd_pfpwd' ).
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.


  method get_timestamp.
    try.
        clear rv_timestamp.

        if mv_header_ts is not initial.
          try.
              data(lv_month) = to_upper( mv_header_ts+8(3) ).
              data(lv_gmt_date) = |{ mv_header_ts+12(4) }{ cond #( when lv_month = 'JAN' then '01'
                                                                   when lv_month = 'FEB' then '02'
                                                                   when lv_month = 'MAR' then '03'
                                                                   when lv_month = 'APR' then '04'
                                                                   when lv_month = 'MAY' then '05'
                                                                   when lv_month = 'JUN' then '06'
                                                                   when lv_month = 'JUL' then '07'
                                                                   when lv_month = 'AUG' then '08'
                                                                   when lv_month = 'SEP' then '09'
                                                                   when lv_month = 'OCT' then '10'
                                                                   when lv_month = 'NOV' then '11'
                                                                   when lv_month = 'DEC' then '12' ) }{ mv_header_ts+5(2) }|.

              data(lv_gmt_time) = |{ mv_header_ts+17(2) }{ mv_header_ts+20(2) }{ mv_header_ts+23(2) }|.

              data(lv_ts) = value timestampl( ).
              data(lv_tz) = value ttzz-tzone( ).

              convert date lv_gmt_date time lv_gmt_time
                into time stamp lv_ts time zone lv_tz.  " default utc

              lv_tz = sy-zonlo.
              convert time stamp lv_ts time zone lv_tz
                into date data(lv_date) time data(lv_time).
            catch cx_sy_range_out_of_bounds ##no_handler.
              lv_date = sy-datum.
              lv_time = sy-uzeit.
          endtry.
        else.
          lv_date = sy-datum.
          lv_time = sy-uzeit.
        endif.

        rv_timestamp = mv_timestamp = condense( |{ lv_date+6(2) }{ lv_date+4(2) }{ lv_date+0(4) }| &&
                                                |{ lv_time+0(2) }:{ lv_time+2(2) }:{ lv_time+4(2) }| ).
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.


  method initialize.
    try.
        cleanup( ).

        if check_ds_server_status( ).
          get_timestamp( ).
          get_pfxid( ).
          get_pfxpwd( ).
          get_apikey( ).
          get_checksum( ).
        endif.
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.


  method sign.
    try.
        clear:
          et_message,
          rv_signed_pdf_binary_data.

        initialize( ).

        " DS server must be running
        if mv_running = abap_true.

          " at least one kind of pdf data must be supplied
          if iv_pdf_binary_data is not initial or it_smartf_otf_data is not initial.

            " only 1 kind of pdf data must be supplied
            if iv_pdf_binary_data is initial or it_smartf_otf_data is initial.

              " check and format input parameters in api format
              data(ls_ds_parameters) = is_ds_parameters.

              if ls_ds_parameters-sign_loc_p is initial.
                ls_ds_parameters-sign_loc_p = '1'.
              endif.

              " x and y co-ordinates are mandatory to identify the location of placing the DS on the PDF
              if ls_ds_parameters-sign_loc_x is not initial and ls_ds_parameters-sign_loc_y is not initial.

                data(ls_ds_parameters_int) = value mty_ds_parameters_int(
                                               sign_loc = |{ condense( ls_ds_parameters-sign_loc_p ) }| &&
                                                          |[{ condense( ls_ds_parameters-sign_loc_x ) }:| &&
                                                          |{ condense( ls_ds_parameters-sign_loc_y ) }]|
                                               approved_by = condense( to_upper( ls_ds_parameters-approved_by ) ) ).

                data(lv_unsigned_pdf) = value xstring( ).

                if iv_pdf_binary_data is not initial.
                  lv_unsigned_pdf = iv_pdf_binary_data.
                endif.

                " convert otf to pdf binary format
                if it_smartf_otf_data is not initial.
                  try.
                      cl_dpr_pdf_conversion_service=>convert_otf_2_pdf(
                        exporting
                          it_otf_data      = it_smartf_otf_data         " Data in OTF Form
                        importing
                          et_pdf_data      = data(lt_pdf)               " Data in PDF Format (Table)
                          ev_pdf_data      = lv_unsigned_pdf            " Data in PDF Format (XSTRING)
                          ev_pdf_data_size = data(lv_pdf_size) ).       " Size of PDF Data
                    catch cx_dpr_pdf_conversion_error into data(lox_pdf_conv_error). " Development Projects: Error When Converting From PDF Data
                      add_message( exporting iox_exception = lox_pdf_conv_error ).
                  endtry.
                endif.

                if lv_unsigned_pdf is not initial.
                  " call corresponding API based on API type
                  case iv_api_type.
                    when mc_api_type-multipart.
                      rv_signed_pdf_binary_data = sign_multipart(
                                                    exporting
                                                      is_ds_parameters   = ls_ds_parameters_int    " DS mandatory parameters
                                                      iv_pdf_binary_data = lv_unsigned_pdf ).      " Un-Signed PDF binary data
                    when mc_api_type-base64.
                      rv_signed_pdf_binary_data = sign_base64(
                                                    exporting
                                                      is_ds_parameters   = ls_ds_parameters_int
                                                      iv_pdf_binary_data = lv_unsigned_pdf ).
                    when others.
                      add_message( exporting is_symsg = value #( msgno = '003' ) ).
                  endcase.

                  if rv_signed_pdf_binary_data is not initial.
                    if iv_display = abap_true.
                      data(lt_signed_pdf) = cl_bcs_convert=>xstring_to_solix(
                                              exporting
                                                iv_xstring = rv_signed_pdf_binary_data ).

                      cl_gui_frontend_services=>show_document(
                        exporting
                          document_name         = conv #( |{ mv_checksum }.pdf| )           " Default document file name
                          mime_type             = if_rest_media_type=>gc_appl_pdf           " MIME Type
                          data_length           = xstrlen( rv_signed_pdf_binary_data )      " File Length
                          keep_file             = abap_true                                 " Keep Temporary File
                        importing
                          temp_file_path        = data(lv_signed_pdf_file_path)             " If KEEP_FILE='X', full path to temporary file
                        changing
                          document_data         = lt_signed_pdf  " Transfer table
                        exceptions
                          cntl_error            = 1              " Error when calling front-end control or internal error
                          error_no_gui          = 2              " No SAPGUI available (background mode)
                          bad_parameter         = 3              " Invalid input value
                          error_writing_data    = 4              " Error when downloading document content
                          error_starting_viewer = 5              " Cannot launch display application
                          unknown_mime_type     = 6              " Front end does not recognize specified MIME typ
                          not_supported_by_gui  = 7              " Method not supported by client
                          access_denied         = 8              " Operation rejected by front end
                          no_authority          = 9              " Missing authority
                          others                = 10 ).
                      if sy-subrc <> 0.
                        add_message( exporting is_symsg = corresponding #( sy ) ).
                      endif.
                    endif.
                  endif.
                endif.
              else.
                add_message( exporting is_symsg = value #( msgno = '004' ) ).
              endif.
            else.
              add_message( exporting is_symsg = value #( msgno = '002' ) ).
            endif.
          else.
            add_message( exporting is_symsg = value #( msgno = '001' ) ).
          endif.
        else.
          add_message( exporting is_symsg = value #( msgno = '005' ) ).
        endif.

        et_message = get_messages( ).

        cleanup( ).
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.


  method sign_base64.
    try.
        clear rv_signed_pdf_binary_data.

        if iv_pdf_binary_data is not initial and is_ds_parameters-sign_loc is not initial.
          data(lo_rest_client) = create_rest_client(
                                   exporting
                                     iv_api_endpoint_url = mc_api_endpoint_url-base64 ).

          if lo_rest_client is bound.
            " get the rest request object from the rest client
            data(lo_request) = lo_rest_client->if_rest_client~create_request_entity( ).

            if lo_request is bound.
              " set the content type as multipart form data
              lo_request->set_content_type(
                exporting
                  iv_media_type = if_rest_media_type=>gc_appl_json ).

              data:
                begin of ls_request,
                  pfxid          type string,
                  pfxpwd         type string,
                  timestamp      type string,
                  cs             type string,
                  filename       type string,
                  signloc        type string,
                  signannotation type string,
                  filepwd        type string,
                  accessid       type string,
                  contents       type string,
                end of ls_request.

              clear ls_request.
              ls_request = value #( pfxid     = mv_pfxid
                                    pfxpwd    = mv_pfxpwd
                                    timestamp = mv_timestamp
                                    cs        = mv_checksum
                                    filename  = |{ mv_checksum }.pdf|
                                    signloc   = is_ds_parameters-sign_loc
                                    signannotation = |{ cond #( when is_ds_parameters-approved_by is not initial
                                                                then |Approved By:| ) }| &&
                                                                     |{ is_ds_parameters-approved_by }|
                                    filepwd   = ''
                                    accessid  = ''
                                    contents  = cl_http_utility=>encode_x_base64(
                                                  exporting
                                                    unencoded = iv_pdf_binary_data ) ).

              if ls_request is not initial.
                data(lv_request_json) = /ui2/cl_json=>serialize(
                                          exporting
                                            data = ls_request
                                            pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

                lo_request->set_string_data( exporting iv_data = lv_request_json ).

                " send recieve - auto sets header field 'method type' to "POST'
                try.
                    lo_rest_client->if_rest_client~post( exporting io_entity = cast #( lo_request ) ).
                  catch cx_rest_client_exception into data(lox_rest_client).
                    add_message( exporting iox_exception = lox_rest_client ).
                endtry.

                " get rest response oject from the rest client
                data(lo_response) = lo_rest_client->if_rest_client~get_response_entity( ).

                if lo_response is bound.
                  data(lv_status_code) = lo_response->get_header_field(
                                           exporting
                                             iv_name = if_http_header_fields_sap=>status_code ).

                  case lv_status_code.
                    when mc_status_code-ok.  " OK
                      data(lv_response_string) = lo_response->get_string_data( ).

                      if lv_response_string is not initial.
                        rv_signed_pdf_binary_data = cl_http_utility=>decode_x_base64(
                                                      exporting
                                                        encoded = lv_response_string ).
                      endif.
                    when mc_status_code-internal_server_error.  " Error
                      data:
                        begin of ls_error,
                          message type string,
                          status  type string,
                        end of ls_error.

                      lv_response_string = lo_response->get_string_data( ).

                      " json to abap
                      clear ls_error.
                      /ui2/cl_json=>deserialize(
                        exporting
                          json             = lv_response_string                       " JSON string
                          pretty_name      = /ui2/cl_json=>pretty_mode-low_case       " Pretty Print property names
                        changing
                          data             = ls_error ).                              " Data to serialize

                      add_message( exporting iv_text = ls_error-message ).
                    when others.
                  endcase.
                endif.
              endif.
            endif.

            " close the rest client - also closes the http client
            lo_rest_client->if_rest_client~close( ).
          endif.
        endif.
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.


  method sign_multipart.
    try.
        clear rv_signed_pdf_binary_data.

        if iv_pdf_binary_data is not initial and is_ds_parameters-sign_loc is not initial.
          data(lo_rest_client) = create_rest_client(
                                   exporting
                                     iv_api_endpoint_url = mc_api_endpoint_url-multipart ).

          if lo_rest_client is bound.
            " get the rest request object from the rest client
            data(lo_request) = lo_rest_client->if_rest_client~create_request_entity( ).

            if lo_request is bound.
              " set the content type as multipart form data
              lo_request->set_content_type(
                exporting
                  iv_media_type = if_rest_media_type=>gc_multipart_form_data ).

              " use the multipart class to access easier/pre-written/reusable methods for setting form fields and file data
              data(lo_multipart_form_data) = new cl_rest_multipart_form_data( io_entity = lo_request ).

              if lo_multipart_form_data is not initial.
                " set form fields as per api doc
                lo_multipart_form_data->set_form_fields(
                  exporting
                    it_fields = value #( ( name = 'pfxid' value = mv_pfxid )
                                         ( name = 'pfxpwd' value = mv_pfxpwd )
                                         ( name = 'filepwd' value = '' )
                                         ( name = 'signloc' value = is_ds_parameters-sign_loc )
                                         ( name = 'signannotation'
                                           value = |{ cond #( when is_ds_parameters-approved_by is not initial
                                                              then |Approved By:| ) }| &&
                                                                   |{ is_ds_parameters-approved_by }| )
                                         ( name = 'timestamp' value = mv_timestamp )
                                         ( name = 'checksum'  value = mv_checksum )
                                         ( name = 'descriptor' value = '' )
                                         ( name = 'accessid' value = '' ) ) ).

                " set the unsigned pdf binary
                lo_multipart_form_data->set_file(
                  exporting
                    iv_name     = conv #( 'uploadfile' )                                " Form Field Name
                    iv_filename = conv #( |{ mv_checksum }.pdf| )        " File Name
                    iv_type     = if_rest_media_type=>gc_appl_pdf                       " Content Type
                    iv_data     = iv_pdf_binary_data ).                                 " Data

                " write the form fields and pdf binary to the http request object
                lo_multipart_form_data->write_to( exporting io_entity = cast #( lo_request ) ).

                " send recieve - auto sets header field 'method type' to "POST'
                try.
                    lo_rest_client->if_rest_client~post( exporting io_entity = cast #( lo_request ) ).
                  catch cx_rest_client_exception into data(lox_rest_client).
                    add_message( exporting iox_exception = lox_rest_client ).
                endtry.

                " get rest response oject from the rest client
                data(lo_response) = lo_rest_client->if_rest_client~get_response_entity( ).

                if lo_response is bound.
                  data(lv_status_code) = lo_response->get_header_field(
                                           exporting
                                             iv_name = if_http_header_fields_sap=>status_code ).

                  case lv_status_code.
                    when mc_status_code-ok.  " OK
                      data(lv_response_string) = lo_response->get_string_data( ).
                      data(lv_response_binary) = lo_response->get_binary_data( ).
                      data(lv_content_length) = lo_response->get_header_field(
                                                  exporting
                                                    iv_name = if_http_header_fields=>content_length ).

                      rv_signed_pdf_binary_data = lv_response_binary.
                    when mc_status_code-internal_server_error.  " Error
                      data:
                        begin of ls_error,
                          message type string,
                          status  type string,
                        end of ls_error.

                      lv_response_string = lo_response->get_string_data( ).

                      " json to abap
                      clear ls_error.
                      /ui2/cl_json=>deserialize(
                        exporting
                          json             = lv_response_string                       " JSON string
                          pretty_name      = /ui2/cl_json=>pretty_mode-low_case       " Pretty Print property names
                        changing
                          data             = ls_error ).                              " Data to serialize

                      add_message( exporting iv_text = ls_error-message ).
                    when others.
                  endcase.
                endif.
              endif.
            endif.

            " close the rest client - also closes the http client
            lo_rest_client->if_rest_client~close( ).
          endif.
        endif.
      catch cx_root into data(lox_root).
        add_message( exporting iox_exception = lox_root ).
    endtry.
  endmethod.
ENDCLASS.
