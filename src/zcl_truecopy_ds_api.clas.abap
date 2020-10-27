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
  endmethod.
ENDCLASS.
