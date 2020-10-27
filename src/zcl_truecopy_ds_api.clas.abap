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

  class-data MC_MULTIPART_API type CHAR1 value '0' ##NO_TEXT.
  class-data MC_BASE64_API type CHAR1 value '1' ##NO_TEXT.

  methods SIGN
    importing
      value(IS_DS_PARAMETERS) type MTY_DS_PARAMETERS
      value(IV_PDF_BINARY_DATA) type XSTRING optional
      value(IT_SMARTF_OTF_DATA) type TSFOTF optional
      value(IV_API_TYPE) type CHAR1 default MC_MULTIPART_API
      value(IV_DISPLAY) type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_SIGNED_PDF_BINARY_DATA) type XSTRING
    raising
      ZCX_GENERIC .
protected section.
private section.

  methods SIGN_MULTIPART
    importing
      value(IS_DS_PARAMETERS) type MTY_DS_PARAMETERS_INT
      value(IV_PDF_BINARY_DATA) type XSTRING
    returning
      value(RV_SIGNED_PDF_BINARY_DATA) type XSTRING
    raising
      ZCX_GENERIC .
  methods SIGN_BASE64
    importing
      value(IS_DS_PARAMETERS) type MTY_DS_PARAMETERS_INT
      value(IV_PDF_BINARY_DATA) type XSTRING
    returning
      value(RV_SIGNED_PDF_BINARY_DATA) type XSTRING .
  class-methods GET_TIMESTAMP
    returning
      value(RV_TIMESTAMP) type STRING .
  class-methods GET_PFXID
    returning
      value(RV_PFXID) type STRING .
  class-methods GET_PFXPWD
    returning
      value(RV_PFXPWD) type STRING .
  class-methods GET_APIKEY
    returning
      value(RV_APIKEY) type STRING .
ENDCLASS.



CLASS ZCL_TRUECOPY_DS_API IMPLEMENTATION.


  method get_apikey.
    clear rv_apikey.
    rv_apikey = cond #( when zcl_helper=>is_development( )
                          or zcl_helper=>is_quality( )
                          or zcl_helper=>is_sandbox( )
                        then 'BPUCZXPG'
                        else 'prd_apikey'  ).
  endmethod.


  method get_pfxid.
    clear rv_pfxid.

    rv_pfxid = cond #( when zcl_helper=>is_development( )
                         or zcl_helper=>is_quality( )
                         or zcl_helper=>is_sandbox( )
                       then 'samco_indofil'
                       else 'prd_pfxid' ).
  endmethod.


  method get_pfxpwd.
    clear rv_pfxpwd.

    rv_pfxpwd = cond #( when zcl_helper=>is_development( )
                          or zcl_helper=>is_quality( )
                          or zcl_helper=>is_sandbox( )
                        then 'samco'
                        else 'prd_pfpwd' ).
  endmethod.


  method get_timestamp.
    clear rv_timestamp.

    rv_timestamp = condense( |{ sy-datum+6(2) }{ sy-datum+4(2) }| && |{ sy-datum+0(4) }| &&
                             |{ sy-uzeit+0(2) }:{ sy-uzeit+2(2) }:{ sy-uzeit+4(2) }| ).
  endmethod.


  method sign.
    clear rv_signed_pdf_binary_data.

    " at least one kind of pdf data must be supplied
    if iv_pdf_binary_data is initial and it_smartf_otf_data is initial.
      raise exception type zcx_generic message id 'Z_DS' type 'E' number '001'.
    endif.

    " only 1 kind of pdf data must be supplied
    if ( iv_pdf_binary_data is not initial and it_smartf_otf_data is not initial ).
      raise exception type zcx_generic message id 'Z_DS' type 'E' number '002'.
    endif.

    " check and format input parameters in api format
    data(ls_ds_parameters) = is_ds_parameters.

    if ls_ds_parameters-sign_loc_p is initial.
      ls_ds_parameters-sign_loc_p = '1'.
    endif.

    if ls_ds_parameters-sign_loc_x is initial or ls_ds_parameters-sign_loc_y is initial.
      raise exception type zcx_generic message id 'Z_DS' type 'E' number '004'.
    endif.

    data(ls_ds_parameters_int) = value mty_ds_parameters_int(
                                   sign_loc = |{ condense( ls_ds_parameters-sign_loc_p ) }| &&
                                              |[{ condense( ls_ds_parameters-sign_loc_x ) }:| &&
                                              |{ condense( ls_ds_parameters-sign_loc_y ) }]|
                                   approved_by = condense( to_upper( ls_ds_parameters-approved_by ) ) ).

    data(lv_pdf_content) = value xstring( ).

    if iv_pdf_binary_data is not initial.
      lv_pdf_content = iv_pdf_binary_data.
    endif.

    " convert otf to pdf binary format
    if it_smartf_otf_data is not initial.
      data: lv_pdf_size type sood-objlen,
            lt_pdf      type standard table of tline.

      clear lt_pdf.
      call function 'CONVERT_OTF'
        exporting
          format                = 'PDF'
        importing
          bin_file              = lv_pdf_content
          bin_filesize          = lv_pdf_size
        tables
          otf                   = it_smartf_otf_data
          lines                 = lt_pdf
        exceptions
          err_max_linewidth     = 1
          err_format            = 2
          err_conv_not_possible = 3
          err_bad_otf           = 4
          others                = 5.
      if sy-subrc <> 0.
        raise exception type zcx_generic message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.

    if lv_pdf_content is not initial.
      " call corresponding API based on API type
      case iv_api_type.
        when mc_multipart_api.
          try.
              rv_signed_pdf_binary_data = sign_multipart(
                                            exporting
                                              is_ds_parameters          = ls_ds_parameters_int          " DS mandatory parameters
                                              iv_pdf_binary_data        = lv_pdf_content ).       " Un-Signed PDF binary data
            catch zcx_generic into data(lox_generic). " Generic Exception Class
          endtry.
        when mc_base64_api.
        when others.
          raise exception type zcx_generic message id 'Z_DS' type 'E' number '003'.
      endcase.
    endif.
  endmethod.


  method SIGN_BASE64.
  endmethod.


  method sign_multipart.
    clear rv_signed_pdf_binary_data.

    if iv_pdf_binary_data is not initial and is_ds_parameters-sign_loc is not initial.
      cl_http_client=>create_by_url(
        exporting
          url                = conv #( |https://indofil.truecopy.in:443/ws/v1/signpdf| )           " URL
        importing
          client             = data(lo_http_client)        " HTTP Client Abstraction
        exceptions
          argument_not_found = 1             " Communication Parameters (Host or Service) Not Available
          plugin_not_active  = 2             " HTTP/HTTPS Communication Not Available
          internal_error     = 3             " Internal Error (e.g. name too long)
          others             = 4 ).
      if sy-subrc <> 0.
        raise exception type zcx_generic message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      if lo_http_client is bound.
        " cast http client to rest client for access to easier/pre-written/reusable rest method calls
        data(lo_rest_client) = cast if_rest_client( new cl_rest_http_client(
                                                      io_http_client = lo_http_client ) ).

        if lo_rest_client is bound.
          " get the http request object from the rest client
          data(lo_http_request) = lo_rest_client->create_request_entity( ).

          if lo_http_request is bound.
            " set the content type as multipart form data
            lo_http_request->set_content_type(
              exporting
                iv_media_type = if_rest_media_type=>gc_multipart_form_data ).

            " use the multipart class to access easier/pre-written/reusable methods for setting form fields and file data
            data(lo_multipart_form_data) = new cl_rest_multipart_form_data(
                                             io_entity = cast #( lo_http_request ) ).

            if lo_multipart_form_data is not initial.
              try.
                  " set form fields as per api doc
                  lo_multipart_form_data->set_form_fields(
                    exporting
                      it_fields = value #( ( name = 'pfxid' value = get_pfxid( ) )
                                           ( name = 'pfxpwd' value = get_pfxpwd( ) )
                                           ( name = 'filepwd' value = '' )
                                           ( name = 'signloc' value = is_ds_parameters-sign_loc )
                                           ( name = 'signannotation' value = |{ cond #( when is_ds_parameters-approved_by is not initial
                                                                                        then |Approved By:| ) }{ is_ds_parameters-approved_by }| )
                                           ( name = 'timestamp' value = get_timestamp( ) )
                                           ( name = 'checksum'
                                             value = substring( val = cl_nwbc_utility=>to_md5(
                                                                        exporting
                                                                          iv_value = |{ get_apikey( ) }| &&
                                                                                                |{ get_timestamp( ) }| ) off = 0 len = 16 ) )
                                           ( name = 'descriptor' value = '' )
                                           ( name = 'accessid' value = '' ) ) ).

                  " set the unsigned pdf binary
                  lo_multipart_form_data->set_file(
                    exporting
                      iv_name     = conv #( 'uploadfile' )                          " Form Field Name
                      iv_filename = conv #( |{ lo_multipart_form_data->get_form_field( exporting iv_name = 'checksum' ) }| &&
                                            |.{ if_rest_media_type=>gc_appl_pdf }| )                            " File Name
                      iv_type     = if_rest_media_type=>gc_appl_pdf       " Content Type
                      iv_data     = iv_pdf_binary_data ).    " Data

                  " write the form fields and pdf binary to the http request object
                  lo_multipart_form_data->write_to( exporting io_entity = cast #( lo_http_request ) ).

                  " send recieve - auto sets header field 'method type' to "POST'
                  lo_rest_client->post( exporting io_entity = cast #( lo_http_request ) ).

                  " get http response oject from the rest client
                  data(lo_http_response) = lo_rest_client->get_response_entity( ).

                  data(lv_status_code) = lo_http_response->get_header_field(
                    exporting
                      iv_name = if_http_header_fields_sap=>status_code ).

                  case lv_status_code.
                    when if_http_status=>reason_200.  " OK
                      data(lv_response) = lo_http_response->get_string_data( ).
                    when if_http_status=>reason_500.  " Error
                    when others.
                  endcase.
                catch cx_sy_range_out_of_bounds ##no_handler.
              endtry.
            endif.
          endif.
        endif.
      endif.
    endif.
  endmethod.
ENDCLASS.
