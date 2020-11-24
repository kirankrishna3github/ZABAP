*"* use this source file for your ABAP unit test classes

class ltcl_truecopy_ds_api definition for testing
  duration medium
  risk level harmless.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltcl_Truecopy_Ds_Api
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_TRUECOPY_DS_API
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  private section.
    data:
      f_cut type ref to zcl_truecopy_ds_api.  "class under test

    class-methods:
      class_setup,
      class_teardown.

    methods:
      generate_sample_otf
        returning
          value(rt_otf) type tsfotf,

      get_frontend_pdf
        returning
          value(rv_unsigned_pdf) type xstring.

    methods:
      setup,
      teardown.

    methods: sign for testing.
endclass.       "ltcl_Truecopy_Ds_Api


class ltcl_truecopy_ds_api implementation.
  method class_setup.
  endmethod.

  method class_teardown.
  endmethod.

  method setup.
    f_cut = new #( ).
  endmethod.

  method teardown.
  endmethod.

  method generate_sample_otf.
    clear rt_otf.

    data(lv_fm_name) = value rs38l_fnam( ).

    call function 'SSF_FUNCTION_MODULE_NAME'
      exporting
        formname           = conv tdsfname( 'Z6SD001S_TAX_INVOICE_GST_REVIS' )  " Form name
      importing
        fm_name            = lv_fm_name
      exceptions
        no_form            = 1
        no_function_module = 2
        others             = 3.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    if lv_fm_name is not initial.
      data(ls_bil_invoice) = value lbbil_invoice( ).

      data(ls_print_data_to_read) = value lbbil_print_data_to_read( ).

      do.
        assign component sy-index of structure ls_print_data_to_read to field-symbol(<lv>).
        if sy-subrc <> 0.
          exit.
        else.
          <lv> = abap_true.
        endif.
      enddo.

      call function 'LB_BIL_INV_OUTP_READ_PRTDATA'
        exporting
          if_bil_number         = cond nast-objky( when zcl_helper=>is_development( ) then '0101800831'
                                                   when zcl_helper=>is_quality( ) then ''
                                                   when zcl_helper=>is_production( ) then '0262001504' )         " Invoice Key for Data Selection
          is_print_data_to_read = ls_print_data_to_read                   " Select. of Tables to be Compl. for Printing RD00;SmartForms
          if_parvw              = space                                   " Partner function (e.g. SH for ship-to party)
          if_parnr              = space                                   " Output Partner
          if_language           = conv nast-spras( sy-langu )             " Message Language
        importing
          es_bil_invoice        = ls_bil_invoice        " Billing Data: Transfer Structure to Smart Forms
        exceptions
          records_not_found     = 1                     " No invoices exist
          records_not_requested = 2                     " No invoices exist
          others                = 3.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      data: ls_job_op_info type ssfcrescl.

      call function lv_fm_name    " '/1BCDWB/SF00000223'
        exporting
          control_parameters = value ssfctrlop( preview   = abap_false
                                                no_dialog = abap_true
                                                getotf    = abap_true )
          is_bil_invoice     = ls_bil_invoice
          is_nast            = value nast( )
          is_repeat          = conv na_repet( space )
          page               = 1
        importing
          job_output_info    = ls_job_op_info
        exceptions
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          others             = 5.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      data(lv_failed) =
        cl_abap_unit_assert=>assert_not_initial(
          exporting
            act              = ls_job_op_info-otfdata               " Actual Data Object
            msg              = |Smartform OTF generation failed|    " Message in Case of Error
            level            = if_aunit_constants=>critical         " Severity (TOLERABLE, >CRITICAL<, FATAL)
            quit             = if_aunit_constants=>method  ).       " Alter control flow/ quit test (NO, >METHOD<, CLASS)

      rt_otf = ls_job_op_info-otfdata.
    endif.
  endmethod.

  method get_frontend_pdf.
    clear rv_unsigned_pdf.

    try.
        zcl_helper=>read_file_from_path(
          exporting
            iv_filepath    = zcl_helper=>file_selection_dialog(
                               exporting
                                 iv_window_title = conv #( |Select Y001 Invoice PDF only| )
                                 iv_file_filter = zcl_truecopy_ds_api=>mc_file_filter_pdf )    " File path on frontend/app server
          importing
            ev_file_length = data(lv_pdf_size) " Binary file length
          receiving
            rt_data        = data(lt_unsigned_pdf) ).
      catch zcx_generic into data(lox_generic). " Generic Exception Class
    endtry.

    data(lv_failed) =
        cl_abap_unit_assert=>assert_not_initial(
          exporting
            act              = lt_unsigned_pdf                            " Actual Data Object
            msg              = |Unsigned PDF selection failed|          " Message in Case of Error
            level            = if_aunit_constants=>critical             " Severity (TOLERABLE, >CRITICAL<, FATAL)
            quit             = if_aunit_constants=>method  ).           " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    rv_unsigned_pdf = cl_bcs_convert=>solix_to_xstring(
                        exporting
                          it_solix   = lt_unsigned_pdf   " Input data
                          iv_size    = lv_pdf_size ).
  endmethod.

  method sign.
*--------------------------------------------------------------------*
    data(lt_message) = value string_table( ).
    data(lt_smf_otf) = generate_sample_otf( ).

    " mulitpart api with display - pdf generated from smartform otf
    data(lv_signed_pdf) = f_cut->sign(
                            exporting                                  " helper method to count number of PDF pages
                              is_ds_parameters = value #( sign_loc_p = zcl_truecopy_ds_api=>get_num_of_pdf_pages(
                                                                         exporting
                                                                           it_smartf_otf_data = lt_smf_otf )
                                                          sign_loc_x = '600'
                                                          sign_loc_y = '50'
                                                          approved_by = 'Mr. Pramod Kadam' )
                                              " helper method to convert otf to pdf
                              iv_pdf_binary = zcl_truecopy_ds_api=>otf_to_pdf(
                                                exporting
                                                  it_smartf_otf_data = lt_smf_otf )
                              " default api type is multipart(iv_api_type)
                              " signed pdf will be displayed by default on the frontend. Can be avoided using iv_display = abap_false
                            importing
                              et_message = lt_message ).

    new zcl_email( )->send_email(
      exporting
        subject       = conv #( 'Signed PDF Test' )             " Email subject
        sender        = conv #( sy-uname )                      " Email Sender - Can be user id or email address directly
        body          = value #( ( line = 'PFA' ) )             " Email Body - table of char255
        body_obj_type = 'RAW' " Code for document class
        recipients    = value #( ( recipient = sy-uname ) )     " Email recipients
        attachments   = value #( ( att_type = 'BIN'
                                   att_subj = 'Signed.PDF'
                                   att_solix = cl_bcs_convert=>xstring_to_solix(
                                                 exporting
                                                   iv_xstring = lv_signed_pdf )
                                   att_size = xstrlen( lv_signed_pdf ) ) )        " For multiple attachments
      importing
        sent          = data(lv_sent) ).          " Email Sent?

    data(lv_failed) =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = lv_signed_pdf                                   " Actual Data Object
          msg              = |Smartform OTF DS with multipart API failed|    " Message in Case of Error
          level            = if_aunit_constants=>critical                    " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no  ).                      " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*--------------------------------------------------------------------*
    " base64 api with display - pdf from frontend
    clear:
      lv_signed_pdf,
      lt_message.

    data(lv_unsigned_pdf) = get_frontend_pdf( ).

    lv_signed_pdf = f_cut->sign(
                      exporting                                  " helper method to count number of PDF pages
                        is_ds_parameters = value #( sign_loc_p = zcl_truecopy_ds_api=>get_num_of_pdf_pages(
                                                                    exporting
                                                                      iv_pdf_binary = lv_unsigned_pdf )
                                                    sign_loc_x = '600'
                                                    sign_loc_y = '50'
                                                    approved_by = 'Mr. Pramod Kadam' )
                                        " helper method to convert otf to pdf
                        iv_pdf_binary = lv_unsigned_pdf
                        iv_api_type = zcl_truecopy_ds_api=>mc_api_type-base64     " for using the base64 api
                        " signed pdf will be displayed by default on the frontend. Can be avoided using iv_display = abap_false
                        iv_print_dialog = abap_true
                      importing
                        et_message = lt_message ).

    lv_failed =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = lv_signed_pdf                                   " Actual Data Object
          msg              = |Frontend PDF with base64 API failed|    " Message in Case of Error
          level            = if_aunit_constants=>critical                    " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no  ).                      " Alter control flow/ quit test (NO, >METHOD<, CLASS)
*--------------------------------------------------------------------*
  endmethod.
endclass.
