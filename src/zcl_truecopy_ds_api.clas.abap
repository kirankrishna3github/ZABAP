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
      value(IV_PDF_BINARY_DATA) type XSTRING .
  methods SIGN_BASE64 .
ENDCLASS.



CLASS ZCL_TRUECOPY_DS_API IMPLEMENTATION.


  method sign.
    clear rv_signed_pdf_binary_data.

    " at least one kind of pdf data must be supplied
    if iv_pdf_binary_data is initial and it_smartf_otf_data is initial.
      raise exception type zcx_generic message id 'Z_DS' type 'E' number '001'.
    endif.

    if ( iv_pdf_binary_data is not initial and it_smartf_otf_data is not initial ).
      raise exception type zcx_generic message id 'Z_DS' type 'E' number '002'.
    endif.

    data(ls_ds_parameters) = is_ds_parameters.

    if ls_ds_parameters-sign_loc_p is initial.
      ls_ds_parameters-sign_loc_p = '1'.
    endif.

    if ls_ds_parameters-sign_loc_x is initial or ls_ds_parameters-sign_loc_y is initial.
      raise exception type zcx_generic message id 'Z_DS' type 'E' number '004'.
    endif.

    data(ls_ds_parameters_int) = value mty_ds_parameters_int(
                                   sign_loc = |{ ls_ds_parameters-sign_loc_p }| &&
                                              |[{ ls_ds_parameters-sign_loc_x }:{ ls_ds_parameters-sign_loc_y }]| ).

    data(lv_pdf_content) = value xstring( ).

    if iv_pdf_binary_data is not initial.
      lv_pdf_content = iv_pdf_binary_data.
    endif.

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
      case iv_api_type.
        when mc_multipart_api.
        when mc_base64_api.
        when others.
          raise exception type zcx_generic message id 'Z_DS' type 'E' number '003'.
      endcase.
    endif.
  endmethod.


  method SIGN_BASE64.
  endmethod.


  method SIGN_MULTIPART.
  endmethod.
ENDCLASS.
