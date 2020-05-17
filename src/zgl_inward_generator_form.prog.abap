*&---------------------------------------------------------------------*
*& Include          ZDM_INWARD_GENERATOR_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZGSP_INWARD_GENERATOR_FORM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
*     I_STRUCTURE_NAME       = 'ZDM_GLOBAL'
      is_layout              = gs_str_layout
      it_fieldcat            = it_fcat
    TABLES
      t_outtab               = gt_final_inward.
  IF sy-subrc <> 0 ##FM_SUBRC_OK.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " DISPLAY_DATA

FORM get_data.
  SELECT  * FROM zdmdoctype
   INTO TABLE l_tab_doctype WHERE
   supplytype  = 'I'.

  SELECT * FROM zei_bus_id
    INTO TABLE l_tab_bus_id.

  SELECT * FROM zei_bus_unit_id
    INTO TABLE l_tab_bus_unit_id.

  SELECT * FROM zei_state_code
     INTO TABLE l_tab_zgst_state_code.

  SELECT * FROM zei_uom_maping
    INTO TABLE l_tab_zei_uom_maping.
  IF l_tab_doctype IS NOT INITIAL.
    SELECT bukrs
          belnr
          gjahr
          monat

          waers awkey xblnr budat bldat blart kursf awtyp xreversal tcode

         FROM bkpf
         INTO TABLE l_tab_bkpf
       FOR ALL ENTRIES IN l_tab_doctype
         WHERE budat IN s_date  "replacement of budat(Posting date with bldat(Document date)
         AND blart = l_tab_doctype-blart(2)
          AND bukrs IN s_bukrs

*          AND XBLNR IN S_XBLNR
           AND stblg = ''.

    SELECT xblnr
     bukrs
     belnr
     gjahr
     budat
     bldat
     blart bupla stblg FROM rbkp INTO TABLE l_tab_rbkp
      FOR ALL ENTRIES IN l_tab_doctype
           WHERE budat IN s_date AND
                bukrs IN s_bupla AND

                 bupla IN s_bupla AND
                 blart = l_tab_doctype-blart(2).

    SORT l_tab_bkpf BY xblnr.
    LOOP AT l_tab_bkpf INTO l_str_bkpf.
*    -------------on 26.09.17
      l_var_awkey = l_str_bkpf-awkey+0(10).
      l_str_bkpf_temp-xblnr = l_str_bkpf-xblnr.
      l_str_bkpf_temp-bukrs = l_str_bkpf-bukrs.
      l_str_bkpf_temp-belnr = l_str_bkpf-belnr.
      l_str_bkpf_temp-gjahr = l_str_bkpf-gjahr.
      l_str_bkpf_temp-awkey = l_var_awkey.
      l_str_bkpf_temp-awtyp = l_str_bkpf-awtyp.
      l_str_bkpf_temp-budat = l_str_bkpf-budat.
      l_str_bkpf_temp-bldat = l_str_bkpf-bldat.
      l_str_bkpf_temp-blart = l_str_bkpf-blart.
      l_str_bkpf_temp-kursf = l_str_bkpf-kursf.
      l_str_bkpf_temp-xreversal = l_str_bkpf-xreversal.
      l_str_bkpf_temp-tcode  = l_str_bkpf-tcode.

      APPEND l_str_bkpf_temp TO l_tab_bkpf_final.
      CLEAR : l_str_bkpf,l_str_bkpf_temp.
    ENDLOOP.
    LOOP AT l_tab_rbkp INTO l_str_rbkp.
      l_str_bkpf_temp-xblnr = l_str_rbkp-xblnr.
      l_str_bkpf_temp-bukrs = l_str_rbkp-bukrs.
      l_str_bkpf_temp-belnr = l_str_rbkp-belnr.
      l_str_bkpf_temp-gjahr = l_str_rbkp-gjahr.
      l_str_bkpf_temp-budat = l_str_rbkp-budat.
      l_str_bkpf_temp-bldat = l_str_rbkp-bldat.
      l_str_bkpf_temp-blart =  l_str_rbkp-blart.

      APPEND l_str_bkpf_temp TO l_tab_bkpf_final.
      CLEAR : l_str_rbkp,l_str_bkpf_temp.
    ENDLOOP.

    SELECT xblnr bukrs belnr gjahr budat bldat blart stblg
      FROM rbkp INTO TABLE l_tab_rbkp_temp
      FOR ALL ENTRIES IN l_tab_bkpf_final
      WHERE belnr = l_tab_bkpf_final-awkey(10) AND
                            gjahr = l_tab_bkpf_final-gjahr  AND
                            xrech = ' '  AND
                            stblg =  ' ' .
    IF l_tab_rbkp_temp[] IS NOT INITIAL.
      LOOP AT l_tab_bkpf_final INTO l_str_bkpf_final.
        READ TABLE l_tab_rbkp_temp INTO l_str_rbkp WITH KEY belnr = l_str_bkpf_final-awkey(10).
        IF sy-subrc = 0.
          DELETE l_tab_bkpf_final WHERE awkey(10) =  l_str_rbkp-belnr.
          DELETE l_tab_bkpf_final WHERE awkey = l_str_rbkp_temp-stblg.
          CLEAR : l_str_bkpf, l_str_rbkp.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF l_tab_rbkp[] IS NOT INITIAL.
      SELECT  belnr
              gjahr
              buzei
              ebeln
              ebelp
              zekkn
              matnr
              bukrs
              werks
              wrbtr
              shkzg
              mwskz
              menge
              bstme
              pstyp
              bklas
              tbtkz
              lfbnr
              lfgja
              lifnr
              customs_val
              FROM rseg   INTO TABLE l_tab_rseg FOR ALL ENTRIES IN l_tab_rbkp
                                                WHERE belnr = l_tab_rbkp-belnr
                                                AND   gjahr = l_tab_rbkp-gjahr.
    ENDIF.

    LOOP AT l_tab_rbkp INTO l_str_rbkp.
      l_str_awkey-belnr = l_str_rbkp-belnr.
      CONCATENATE l_str_rbkp-belnr l_str_rbkp-gjahr INTO l_str_awkey-awkey.
      APPEND l_str_awkey TO l_tab_awkey.
      CLEAR: l_str_awkey.
    ENDLOOP.

    IF l_tab_bkpf_final IS NOT INITIAL.
      SELECT bukrs
             belnr
             gjahr
             buzei
             buzid
             koart
             mwskz
             txgrp
             ktosl
             matnr
             werks
             menge
             meins
             bustw
             taxps
             bupla
             lifnr
             FROM bseg INTO TABLE l_tab_bseg FOR ALL ENTRIES IN l_tab_bkpf_final
                                             WHERE bukrs = l_tab_bkpf_final-bukrs
                                             AND   belnr = l_tab_bkpf_final-belnr
                                             AND   gjahr = l_tab_bkpf_final-gjahr
                                             AND  bupla IN s_bupla.
      " and matnr ne '  '..
      SELECT bukrs
                belnr
                gjahr
                buzei
                buzid
                koart
                mwskz
                txgrp
                ktosl
                matnr
                werks
                menge
                meins
                bustw
                taxps
                bupla
                lifnr
                FROM bseg INTO TABLE l_tab_bseg_matnr FOR ALL ENTRIES IN l_tab_bseg
                                                WHERE bukrs = l_tab_bseg-bukrs
                                                AND   belnr = l_tab_bseg-belnr
                                                AND   gjahr = l_tab_bseg-gjahr
                                                AND  matnr <> ' ' .


      SELECT bukrs
         belnr
         gjahr
         buzei
         buzid
         koart
         mwskz
         txgrp
         ktosl
         matnr
         werks
         menge
         meins
         bustw
         taxps
         bupla
         lifnr
         zuonr

         FROM bseg INTO TABLE l_tab_bseg1 FOR ALL ENTRIES IN l_tab_bkpf_final
                                         WHERE bukrs = l_tab_bkpf_final-bukrs
                                         AND   belnr = l_tab_bkpf_final-belnr
                                         AND   gjahr = l_tab_bkpf_final-gjahr
                                         AND  bupla IN s_bupla
                                         AND ktosl = ' '
                                         AND mwskz <> 'V0'
                                      .



*                                           AND   ktosl = 'WRX'.

      SELECT bukrs
         belnr
         gjahr
         buzei
         buzid
         koart
         mwskz
         txgrp
         ktosl
         matnr
         werks
         menge
         meins
         bustw
         taxps
         bupla
         lifnr
         FROM bseg INTO TABLE l_tab_bseg_lifnr FOR ALL ENTRIES IN l_tab_bkpf_final
                                         WHERE bukrs = l_tab_bkpf_final-bukrs
                                         AND   belnr = l_tab_bkpf_final-belnr
                                         AND   gjahr = l_tab_bkpf_final-gjahr
                                         AND lifnr NE ' ' .


      SELECT bukrs
         belnr
         gjahr
         buzei
         buzid
         koart
         mwskz
         txgrp
         ktosl
         matnr
         werks
         menge
         meins
         bustw
         taxps
         bupla
         lifnr
         FROM bseg INTO TABLE l_tab_bseg_menge FOR ALL ENTRIES IN l_tab_bkpf_final
                                         WHERE bukrs = l_tab_bkpf_final-bukrs
                                         AND   belnr = l_tab_bkpf_final-belnr
                                         AND   gjahr = l_tab_bkpf_final-gjahr
                                         AND menge NE ' ' .

      SELECT bukrs
         belnr
         gjahr
         buzei
         buzid
         koart
         mwskz
         txgrp
         ktosl
         matnr
         werks
         menge
         meins
         bustw
         taxps
         bupla
         lifnr
         zuonr
         hsn_sac
         FROM bseg INTO TABLE l_tab_bseg_hsn FOR ALL ENTRIES IN l_tab_bkpf_final
                                         WHERE bukrs = l_tab_bkpf_final-bukrs
                                         AND   belnr = l_tab_bkpf_final-belnr
                                         AND   gjahr = l_tab_bkpf_final-gjahr
                                         AND hsn_sac NE ' '.
    ENDIF.

    IF l_tab_bseg[] IS NOT INITIAL.

      SELECT matnr maktx FROM makt INTO TABLE l_tab_makt
        FOR ALL ENTRIES IN l_tab_bseg
        WHERE matnr = l_tab_bseg-matnr.


      SELECT bukrs
             belnr
             gjahr

             txgrp
          buzei
             shkzg
             fwbas
             fwste
             kschl
             kbetr
             mwskz
             FROM  bset INTO TABLE l_tab_bset FOR ALL ENTRIES IN l_tab_bseg
                                              WHERE bukrs = l_tab_bseg-bukrs
                                              AND   belnr = l_tab_bseg-belnr
                                              AND   gjahr = l_tab_bseg-gjahr.
*                                            and kbetr ne 0.


    ENDIF.
    IF l_tab_bseg_lifnr IS NOT INITIAL.
      SELECT bukrs branch gstin name FROM j_1bbranch
        INTO TABLE l_tab_j_1bbranch FOR ALL ENTRIES IN l_tab_bseg_lifnr
        WHERE branch = l_tab_bseg_lifnr-bupla.
      SELECT lifnr
     land1
     name1
     name2
     name3
     name4
     ort01
     regio
     stceg
     stcd3 adrnr FROM lfa1 INTO TABLE l_tab_lfa1 FOR ALL ENTRIES IN l_tab_bseg_lifnr
                                           WHERE lifnr = l_tab_bseg_lifnr-lifnr.
    ENDIF.

    SELECT addrnumber name3 FROM adrc INTO TABLE l_tab_adrc  FOR ALL ENTRIES IN l_tab_lfa1
      WHERE addrnumber = l_tab_lfa1-adrnr.
  ENDIF.

  SORT l_tab_rseg BY ebeln ebelp.
  SORT l_tab_rbkp BY belnr gjahr.
  SORT l_tab_lfa1 BY lifnr.
  SORT l_tab_awkey BY belnr.
  SORT l_tab_bkpf BY bukrs belnr gjahr ."awkey.
  SORT l_tab_bseg BY bukrs belnr gjahr buzei ."mwskz.
  SORT l_tab_bseg1 BY belnr mwskz.
  SORT l_tab_bset BY bukrs belnr gjahr   txgrp buzei shkzg fwbas fwste kschl .
  SORT l_tab_bseg_lifnr BY bukrs belnr gjahr.
  SORT l_tab_bseg_hsn BY bukrs belnr gjahr buzei.
  SORT l_tab_bseg_menge BY bukrs belnr gjahr buzei.
  SORT l_tab_j_1bbranch BY bukrs branch gstin.
  SORT l_tab_adrc BY addrnumber.
  SORT l_tab_bseg_matnr BY bukrs belnr buzei.
  SORT l_tab_makt BY matnr.
*  sort l_Tab_vbrk by vbeln .
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*       textsignet
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_download.
  TYPES:
    BEGIN OF ty_out_invoice,
      custom12                 TYPE char50, " Business Place
      location_gstin           TYPE char50, "g
      bill_from_oth_party_name TYPE char50,
      bill_from_gstin          TYPE char50,
      pos                      TYPE char50,
      exp_acc_desc             TYPE char50,
      gstin_of_ecomm           TYPE char50,
      legal_name_ecomm         TYPE char50, "g
      document_type            TYPE char50,
      custom10                 TYPE char50,
      document_no              TYPE char50,
      document_date            TYPE char50,
      custom7                  TYPE char50,
      transaction_type         TYPE char50,
      elig_itc                 TYPE char50,
      port_code                TYPE char50,
      ship_bill_no             TYPE char50,
      ship_bill_date           TYPE char50,
      ship_bill_value          TYPE char50,
      cgst_rate                TYPE char50,
      sgst_rate                TYPE char50,
      igst_rate                TYPE char50,
      consolidated_rate        TYPE char50,
      cess_rate                TYPE char50,
      tax_value                TYPE char50,
      discount_rate            TYPE char50,
      discount_amount          TYPE char50,
      gross_tax_amount         TYPE char50,
      custom9                  TYPE char50,
      cgst_amount              TYPE char50,
      sgst_amount              TYPE char50,
      igst_amount              TYPE char50,
      cess_amt                 TYPE char50,
      rev_charges              TYPE char50,
      elig_itc2                TYPE char50,
      cent_itc_amt             TYPE char50,
      ut_itc_amt               TYPE char50,
      intr_itc_amt             TYPE char50,
      cess_itc_amt             TYPE char50,
      document_value           TYPE char50,
      orig_doc_no              TYPE char50,
      orig_doc_date            TYPE char50,
      orig_crdr_no             TYPE char50,
      orig_crdr_date           TYPE char50,
      hsn                      TYPE char50,
      uom                      TYPE char50,
      quantity                 TYPE char50,
      rate_per_unit            TYPE char50,
      cancel_doc               TYPE char50,
      custom1                  TYPE char50,
      document_date2           TYPE char50,
      custom8                  TYPE char50,
      custom19                 TYPE char50,
      custom21                 TYPE char50,   " Custom1
      custom22                 TYPE char50,
      custom23                 TYPE char50,
      custom24                 TYPE char50,
      custom25                 TYPE char50,

    END OF ty_out_invoice.

***Internal Tables******************************************************
  DATA : l_tab_final          TYPE STANDARD TABLE OF ty_out_invoice,
         l_tab_data           TYPE TABLE OF text,
         l_tab_inward_invoice TYPE truxs_t_text_data.
***Structure************************************************************
  DATA : l_str_out_invoice TYPE ty_final_inward,
*         l_tab_out_invoice TYPE STANDARD TABLE OF ty_out_invoice,
         l_str_final       TYPE ty_out_invoice,
         l_str_date        TYPE range_s_dats,
         ls_cust_data      TYPE zgsp_cust_data,
         lv_tsl            TYPE timestampl,
         lv_time_stamp     TYPE char21.
***Variables************************************************************
  DATA : l_var_pwd_len    TYPE i, "For finding the length of the passward, This is used when scrambling the passward
         l_var_handle     TYPE i, "Handle for Pointing to an already connected FTP connection,used for subsequent actions on the connected FTP session
         l_var_path(100),"Path that points to the FTP User's Home Directory
         l_var_tsl        TYPE timestampl, "Time Stamp
         l_var_time_stamp TYPE char21, "Time Stamp
         l_var_bukrs      TYPE bukrs,
         l_var_answer     TYPE char1,
         l_var_date       TYPE sy-datum,
***         l_var_pwd_len  TYPE i,
***         l_var_handle   TYPE i,
         g_var_response   TYPE char1.
***         l_tab_data      TYPE TABLE OF text,9339906838
***         l_var_path(100) TYPE c.
***Constant*************************************************************
  CONSTANTS : l_con_key       TYPE i VALUE 26101957. "Hardcoded Handler Key,This is always '26101957'
************************************************************************
  IF sy-batch = abap_false.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmation'
        text_question         = 'Are you want to confirm create the file on FTP?'
        text_button_1         = 'Yes'
        text_button_2         = 'No'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = l_var_answer.
    IF l_var_answer = 2.
**      MESSAGE E004(ZGSP_UTILITY).
    ENDIF.
  ENDIF.
  IF gt_final_inward IS NOT INITIAL.
    SORT gt_final_inward BY location_gstin document_no.
***    AUTHORITY-CHECK OBJECT 'S_ADMI_FCD'
***         ID 'S_ADMI_FCD'
***         FIELD 'SFTP'.
***    IF sy-subrc <> 0.
***      MESSAGE 'no_authorization' TYPE 'E'.
***      EXIT.
****     Keine Berechtigung zum Ändern der Aktivierung einer ID
***    ENDIF.
    SORT gt_final_inward BY location_gstin document_no.
    SELECT SINGLE mandt
           file_type
           FILE_TYPE_DISC
           ftp_host
           ftp_user
           ftp_passward
           ftp_extension
           rfc_destination
           request_cammand
           req_com_arch
           response_all_file_cmd
           response_command FROM zgsp_cust_data INTO ls_cust_data WHERE file_type = '04'.
    l_var_pwd_len = strlen( ls_cust_data-ftp_passward ).
    CALL FUNCTION 'HTTP_SCRAMBLE' "For Encrypting the passward
      EXPORTING
        source      = ls_cust_data-ftp_passward
        sourcelen   = l_var_pwd_len
        key         = l_con_key
      IMPORTING
        destination = ls_cust_data-ftp_passward.

    CALL FUNCTION 'FTP_CONNECT' "For connecting to the FTP Server's user directory
      EXPORTING
        user            = ls_cust_data-ftp_user
        passward        = ls_cust_data-ftp_passward
        host            = ls_cust_data-ftp_host
        rfc_destination = ls_cust_data-rfc_destination
      IMPORTING
        handle          = l_var_handle
      EXCEPTIONS
        not_connected   = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      g_var_response = 1.
      RETURN.
    ELSE.
      CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
          handle        = l_var_handle
          command       = ls_cust_data-request_cammand
        TABLES
          data          = l_tab_data
        EXCEPTIONS
          command_error = 1
          tcpip_error   = 2.
      IF sy-subrc <> 0.
        g_var_response = 2.
        CALL FUNCTION 'FTP_DISCONNECT' "For Disconnecting the connected FTP Session
          EXPORTING
            handle = l_var_handle
          EXCEPTIONS
            OTHERS = 1.

        CALL FUNCTION 'RFC_CONNECTION_CLOSE'
          EXPORTING
            destination = ls_cust_data-rfc_destination
          EXCEPTIONS
            OTHERS      = 1.
        RETURN.
      ENDIF.
    ENDIF.

    TYPES : BEGIN OF ty_final_inward1.
    TYPES : location_gstin1 TYPE ZDM_DET_LOCATION_GSTIN.
        INCLUDE STRUCTURE zdm_global.
    TYPES : END OF ty_final_inward1.

    DATA : gt_final_inward1   TYPE TABLE OF ty_final_inward1,
           l_str_out_invoice1 TYPE ty_final_inward1.

    LOOP AT gt_final_inward INTO gs_final_inward.
      MOVE-CORRESPONDING  gs_final_inward TO l_str_out_invoice1.
      l_str_out_invoice1-location_gstin1   = gs_final_inward-location_gstin.
      APPEND l_str_out_invoice1 TO gt_final_inward1.
      CLEAR:l_str_out_invoice1.
    ENDLOOP.
    DATA: lv_gstin TYPE kna1-stcd3.
*********************************************************************
    SORT gt_final_inward1 BY location_gstin1.
    LOOP AT gt_final_inward1 INTO l_str_out_invoice1.
      AT NEW location_gstin1.
        l_str_final-custom12                   =  'Business Place'.
        l_str_final-location_gstin             =  'Location GSTIN'.
        l_str_final-bill_from_oth_party_name   =  'Vendor Name'.
        l_str_final-bill_from_gstin            =  'Vendor GSTIN'.
        l_str_final-pos                        =  'Place Of Supply'.
        l_str_final-exp_acc_desc               =  'Expense Account Description'.
        l_str_final-gstin_of_ecomm             =  'GSTIN of Ecommerce Operator'.
        l_str_final-legal_name_ecomm           =  'Legal Name of Ecommerce Operator'.
        l_str_final-document_type              =  'Document Type'.
        l_str_final-custom10                   =  'PO Number'.
        l_str_final-document_no                =  'Vendor Invoice Number'.
        l_str_final-document_date              =  'Vendor Invoice Date'.
        l_str_final-custom7                    =  'Posting Date'.
        l_str_final-transaction_type            =  'Invoice Type'.
        l_str_final-elig_itc                    =  'Nature of Goods'.
        l_str_final-port_code                   =  'Port Code'.
        l_str_final-ship_bill_no                =  'Bill of Entry Number'.
        l_str_final-ship_bill_date              =  'Bill of Entry Date'.
        l_str_final-ship_bill_value             =  'Bill of Entry Value'.
        l_str_final-cgst_rate                   =  'CGST Rate'.
        l_str_final-sgst_rate                   =  'SGST Rate'.
        l_str_final-igst_rate                   =  'IGST Rate'.
        l_str_final-consolidated_rate           =  'Consolidated Rate/Rate (in %)'.
        l_str_final-cess_rate                   =  'CESS Rate'.
        l_str_final-tax_value                   =  'Taxable Value'.
        l_str_final-discount_rate               =  'Discounts in %'.
        l_str_final-discount_amount             =  'Discounts in Amount'.
        l_str_final-gross_tax_amount            =  'Gross Taxable Value'.
        l_str_final-custom9                     =  'Tax Code'.
        l_str_final-cgst_amount                 =  'CGST Amount'.
        l_str_final-sgst_amount                 =  'SGST Amount'.
        l_str_final-igst_amount                 =  'IGST Amount'.
        l_str_final-cess_amt                    =  'CESS Amount'.
        l_str_final-rev_charges                 =  'RCM Applicability'.
        l_str_final-elig_itc2                   =  'ITC Eligibility'.
        l_str_final-cent_itc_amt                =  'Eligible CGST'.
        l_str_final-ut_itc_amt                  =  'Eligible SGST'.
        l_str_final-intr_itc_amt                =  'Eligible IGST'.
        l_str_final-cess_itc_amt                =  'Eligible CESS'.
        l_str_final-document_value               =  'Invoice Value'.
        l_str_final-orig_doc_no                  =  'Original Invoice No.'.
        l_str_final-orig_doc_date                =  'Original Invoice Date'.
        l_str_final-orig_crdr_no                 =  'Original Debit-Credit Note Number / Payment Note Number'.
        l_str_final-orig_crdr_date               =  'Original Debit-Credit Note Date / Payment Note Date'.
        l_str_final-hsn                          =  'HSN Code'.
        l_str_final-uom                          =  'Unit of Measurement'.
        l_str_final-quantity                     =  'Quantity'.
        l_str_final-rate_per_unit                =  'Rate per Unit'.
        l_str_final-cancel_doc                   =  'Cancelled Doc?'.
        l_str_final-custom1                      =  'Document Number'.
        l_str_final-document_date2               =  'Document Date'.
        l_str_final-custom8                      =  'G/L Account'.
        l_str_final-custom19                     =  'Expense G/L Account Description'.
        l_str_final-custom21                     =  'Custom Field 1 '.
        l_str_final-custom22                     =  'Custom Field 2'.
        l_str_final-custom23                     =  'Custom Field 3 '.
        l_str_final-custom24                     =  'Custom Field 4 '.
        l_str_final-custom25                     =  'Custom Field 5 '.
        APPEND l_str_final TO l_tab_final.
        CLEAR: l_str_final.
      ENDAT.
      MOVE-CORRESPONDING l_str_out_invoice1 TO l_str_final.
      l_str_final-elig_itc2   = l_str_out_invoice1-elig_itc.
      CLEAR:l_str_final-elig_itc.

*      READ TABLE gt_final_inward_ey2 INTO ls_final_inward_temp_ey WITH KEY documentnumber  = l_str_out_invoice1-custom1.
*      IF sy-subrc = 0.
*        l_str_final-document_date2 = ls_final_inward_temp_ey-documentdate.
*      ENDIF.


      IF l_str_out_invoice1-cgst_rate IS INITIAL.
        CLEAR : l_str_final-cgst_rate.
      ENDIF.
      IF l_str_out_invoice1-cgst_amount IS INITIAL.
        CLEAR : l_str_final-cgst_amount.
      ENDIF.
      IF l_str_out_invoice1-sgst_rate IS INITIAL.
        CLEAR : l_str_final-sgst_rate.
      ENDIF.
      IF l_str_out_invoice1-sgst_amount IS INITIAL.
        CLEAR : l_str_final-sgst_amount.
      ENDIF.
      IF l_str_out_invoice1-igst_rate IS INITIAL.
        CLEAR : l_str_final-igst_rate.
      ENDIF.
      IF l_str_out_invoice1-igst_amount IS INITIAL.
        CLEAR : l_str_final-igst_amount.
      ENDIF.

      IF l_str_out_invoice1-pos = l_str_out_invoice1-bill_to_state.
        IF l_str_out_invoice1-cgst_rate IS INITIAL AND l_str_out_invoice1-sgst_rate IS INITIAL AND l_str_out_invoice1-igst_rate IS INITIAL.
          l_str_final-cgst_rate = 0.
          l_str_final-cgst_amount = 0.
          l_str_final-sgst_rate = 0.
          l_str_final-sgst_amount = 0.
        ENDIF.
      ENDIF.
      IF l_str_out_invoice1-pos <> l_str_out_invoice1-bill_to_state.
        IF l_str_out_invoice1-sgst_rate IS INITIAL AND l_str_out_invoice1-cgst_rate IS NOT INITIAL AND l_str_out_invoice1-igst_rate IS INITIAL.
          l_str_final-igst_rate = 0.
          l_str_final-igst_amount = 0.
        ENDIF.
      ENDIF.
      IF l_str_out_invoice1-quantity IS INITIAL.
        l_str_final-quantity = 1.
      ENDIF.

      IF l_str_out_invoice1-pos = 99 AND ( l_str_out_invoice1-sub_supply_type = 'EXPWP' OR l_str_out_invoice1-sub_supply_type = 'EXPWOP' ).
        l_str_final-igst_rate = 0.
        l_str_final-igst_amount = 0.
        CLEAR: l_str_final-cgst_rate,l_str_final-cgst_amount,l_str_final-sgst_amount,l_str_final-sgst_rate.
      ENDIF.

*      IF L_STR_OUT_INVOICE1-diff_percentage IS INITIAL.
*        CLEAR:l_str_final-diff_percentage.
*      ENDIF.
      IF l_str_out_invoice1-cess_rate IS INITIAL.
        CLEAR:l_str_final-cess_rate.
      ENDIF.

      IF l_str_out_invoice1-cess_amt IS INITIAL.
        CLEAR:l_str_final-cess_amt.
      ENDIF.

      IF l_str_out_invoice1-elig_itc IS INITIAL.
        CLEAR:l_str_final-intr_itc_amt.
        CLEAR:l_str_final-cent_itc_amt.
        CLEAR:l_str_final-ut_itc_amt.
        CLEAR:l_str_final-ut_itc_amt.
        CLEAR:l_str_final-cess_itc_amt.
      ENDIF.

      IF l_str_out_invoice1-discount_amount IS INITIAL.
        CLEAR:l_str_final-discount_amount.
      ENDIF.

*****Negative Sign position change form last to first
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = l_str_final-tax_value.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = l_str_final-igst_amount.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = l_str_final-cgst_amount.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = l_str_final-sgst_amount.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = l_str_final-cess_amt.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = l_str_final-document_value.


      IF l_str_out_invoice1-document_date IS NOT INITIAL.
        CONCATENATE l_str_out_invoice1-document_date+6(2) '-' l_str_out_invoice1-document_date+4(2) '-' l_str_out_invoice1-document_date+0(4)
                    INTO l_str_final-document_date.
      ELSE.
        CLEAR:l_str_final-document_date.
      ENDIF.

      IF l_str_final-document_date2 IS NOT INITIAL.
        CONCATENATE l_str_final-document_date2+8(2) '-' l_str_final-document_date2+5(2) '-' l_str_final-document_date2+0(4)
                    INTO l_str_final-document_date2.
      ELSE.
        CLEAR:l_str_final-document_date2.
      ENDIF.

      IF l_str_out_invoice1-custom7 IS NOT INITIAL.
        CONCATENATE l_str_out_invoice1-custom7+0(2) '-' l_str_out_invoice1-custom7+3(2) '-' l_str_out_invoice1-custom7+6(4)
                    INTO l_str_final-custom7.
      ELSE.
        CLEAR:l_str_final-custom7.
      ENDIF.

      IF l_str_out_invoice1-ship_bill_date IS NOT INITIAL.
        CONCATENATE l_str_out_invoice1-ship_bill_date+8(2) '-'
                    l_str_out_invoice1-ship_bill_date+5(2) '-'
                    l_str_out_invoice1-ship_bill_date+0(4)
        INTO l_str_final-ship_bill_date.
      ELSE.
        CLEAR: l_str_final-ship_bill_date.
      ENDIF.

      IF l_str_final-document_date = '- -' OR l_str_final-document_date = '--'.
        CLEAR:l_str_final-document_date.
      ENDIF.

      IF l_str_final-document_date2 = '- -' OR l_str_final-document_date2 = '--'.
        CLEAR:l_str_final-document_date2.
      ENDIF.

      IF l_str_final-ship_bill_date = '- -' OR l_str_final-ship_bill_date = '--'.
        CLEAR:l_str_final-ship_bill_date.
      ENDIF.
      lv_gstin  =   l_str_final-location_gstin.
**********************************************************************
      APPEND l_str_final TO l_tab_final.
      CLEAR: l_str_final.


**********************************************************************
      AT END OF location_gstin1.
        IF l_tab_final[] IS NOT INITIAL.
*** Data convert in CSV
          CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
            EXPORTING
              i_field_seperator    = ','
            TABLES
              i_tab_sap_data       = l_tab_final
            CHANGING
              i_tab_converted_data = l_tab_inward_invoice
            EXCEPTIONS
              conversion_failed    = 1
              OTHERS               = 2.
          IF sy-subrc = 0.
            GET TIME STAMP FIELD lv_tsl.
            lv_time_stamp = lv_tsl.
***            lv_path = 'C:\Cygnet\'.
            DATA: test TYPE string."Document_5536_08_2019_090919170404.xlsx "Time Stamp
            CONCATENATE lv_gstin'_'ls_cust_data-FILE_TYPE_DISC '_'s_date-low+4(2) '_' s_date-low+0(4)'_'
            sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) sy-uzeit '.' l_var_time_stamp+14(3)  ls_cust_data-ftp_extension INTO l_var_path .
            CALL FUNCTION 'FTP_R3_TO_SERVER' "For Creating a file from SAP R3 to FTP Server
              EXPORTING
                handle         = l_var_handle
                fname          = l_var_path
                character_mode = abap_true
              TABLES
                text           = l_tab_inward_invoice "Final Internal table to be written to the text file in the FTP Server's Directory
              EXCEPTIONS
                tcpip_error    = 1
                command_error  = 2
                data_error     = 3
                OTHERS         = 4.

            IF sy-subrc <> 0."When FTP connection Fails
              g_var_response = 5.
              EXIT.
            ELSE.
              g_var_response = 0.
              FREE: l_tab_final[], l_tab_inward_invoice[].
            ENDIF.
          ENDIF.
        ELSE.
***          g_var_response = 3.
        ENDIF.
      ENDAT.
      CLEAR : l_str_out_invoice1,lv_gstin.
    ENDLOOP.
    CALL FUNCTION 'FTP_DISCONNECT' "For Disconnecting the connected FTP Session
      EXPORTING
        handle = l_var_handle
      EXCEPTIONS
        OTHERS = 1.

    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
      EXPORTING
        destination = ls_cust_data-rfc_destination
      EXCEPTIONS
        OTHERS      = 1.
****    CALL SCREEN 1000.
  ELSE.
****    g_var_response = 4.
  ENDIF.
ENDFORM.                    " FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*& Form SET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_data .
  LOOP AT l_tab_bkpf INTO l_str_bkpf.
*LOOP at l_tab_bseg  into l_str_bseg.
*  READ TABLE l_tab_bkpf into l_str_bkpf with key bukrs = l_str_bseg-bukrs
*                                                       belnr = L_STR_BSEG-belnr
*                                                       gjahr = l_str_bseg-gjahr BINARY SEARCH.
    IF sy-subrc  = 0.
      l_str_final-document_no = l_str_bkpf-xblnr.
      l_str_final-document_date = l_str_bkpf-budat.
    ENDIF.
    READ TABLE l_tab_bseg_lifnr INTO l_str_bseg_lifnr WITH KEY bukrs = l_str_bkpf-bukrs
                                                              belnr = l_str_bkpf-belnr
                                                              gjahr = l_str_bkpf-gjahr BINARY SEARCH .
    IF sy-subrc = 0.
      READ TABLE l_tab_lfa1 INTO l_str_lfa1 WITH KEY lifnr = l_str_bseg_lifnr-lifnr.
      IF sy-subrc = 0.
        " l_str_final-LOCATION_GSTIN = L_STR_LFA1-stcd3.
        l_str_final-location_legal_name = l_str_lfa1-name1.
        l_str_final-bill_from_gstin = l_str_lfa1-stcd3.
        READ TABLE l_tab_adrc INTO l_str_adrc WITH KEY addrnumber = l_str_lfa1-adrnr BINARY SEARCH.
        IF sy-subrc = 0.
          l_str_final-bill_from_address1 = l_str_adrc-name3.
          l_str_final-bill_from_address2 = l_str_adrc-name3.
        ENDIF.

      ENDIF.
    ENDIF.
    READ TABLE l_tab_bseg INTO l_str_bseg WITH KEY bukrs = l_str_bkpf-bukrs
                                                         belnr = l_str_bkpf-belnr
                                                         gjahr = l_str_bkpf-gjahr BINARY SEARCH.
*   READ TABLE l_tab_bseg_matnr  into l_str_bseg_matnr with key bukrs = l_str_bkpf-bukrs
*                                                       belnr = L_STR_Bkpf-belnr
*                                                       gjahr = l_str_bkpf-gjahr BINARY SEARCH.
*
*    READ TABLE L_tab_makt INTO l_Str_makt with key matnr = l_Str_bseg-matnr BINARY SEARCH.
*    if sy-SUBRC = 0.
*      l_str_final-DESCRIPTION = l_Str_makt-maktx.
*
*      ENDIF.

    l_str_final-document_purpose = 'GST'.
    l_str_final-supply_type = 'I'.
    l_str_final-sub_supply_type = 'SUP'.
    READ TABLE l_tab_doctype INTO l_str_doctype WITH KEY supplytype = 'I'
                                                       blart = l_str_bkpf-blart.
    IF sy-subrc = 0.
      l_str_final-document_type = l_str_doctype-doctype.
    ENDIF.

    IF l_str_final-document_type = 'CRN'.
      l_str_final-reference_doc_type = 'INV'.
*      l_str_final-REFERENCE_DOC_NO
*       l_str_final-REFERENCE_DOC_DATE.
    ENDIF.
    l_str_final-transaction_type = 'REG'.
    l_str_final-custom1 = l_str_bseg-belnr.
    l_str_final-custom2  = l_str_bseg_lifnr-bupla.

*READ TABLE l_tab_bseg_hsn INTO l_str_bseg_hsn with key bukrs = l_str_bkpf-bukrs
*                                                       belnr = l_str_bkpf-belnr
*                                                       gjahr  = l_str_bkpf-gjahr.
*if sy-SUBRC = 0.
*  l_str_final-hsn = l_str_bseg_hsn-hsn_sac.
*  endif.


*quantity logic.
*READ TABLE l_tab_bseg_menge INTO l_str_bseg_menge with key bukrs = l_str_bkpf-bukrs
*                                                       belnr = l_str_bkpf-belnr
*                                                       gjahr  = l_str_bkpf-gjahr
*                                                       buzei = l_str_bseg-buzei .
*if sy-SUBRC  = 0.
*  l_str_final-quantity = l_str_bseg_menge-menge.
*  endif.

*l_str_final-PRODUCT_NAME = l_str_bseg-matnr.
*POS LOGIC
*============
*READ TABLE  l_tab_lfa1 INTO L_STR_LFA1 WITH KEY lifnr = L_STR_BSEG_LIFNR-lifnr.
*if sy-SUBRC = 0.
*  READ TABLE l_tab_zgst_state_code into l_str_zgst_state_code with key regio = L_STR_LFA1-regio.
*  if sy-SUBRC = 0.
*    l_str_final-pos = l_str_zgst_state_code-EWAY_STATE_CODE.
*    ENDIF.
*  ENDIF.

    READ TABLE l_tab_j_1bbranch INTO l_str_j_1bbranch WITH KEY bukrs = l_str_bseg_lifnr-bukrs
                                                               branch = l_str_bseg_lifnr-bupla.
    IF sy-subrc = 0.
      l_str_final-location_gstin  = l_str_j_1bbranch-gstin.
      l_str_final-location_legal_name = l_str_j_1bbranch-name.
      l_str_final-bill_to_gstin  = l_str_j_1bbranch-gstin.
      l_str_final-bill_to_name = l_str_j_1bbranch-name.
    ENDIF.
    l_str_final-pos = l_str_final-location_gstin(2).
    l_str_final-bukrs = l_str_bkpf-bukrs.
    l_str_final-rev_charges = 'N'.
    DATA : l_var_tabix TYPE sy-tabix.
*  loop AT l_tab_Bset INTO l_str_bset.
*     READ TABLE l_tab_bset into l_str_bset  with key  bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr BINARY SEARCH.
*    if sy-SUBRC = 0.
*  READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JICG' BINARY SEARCH." kschl = 'JICR' kschl = 'JOCG'.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-cgst_amount  = l_str_bset-fwste.
*    l_str_final-cent_itc_amt = l_str_final-cgst_amount.
*      l_str_final-cgst_rate =  ( l_str_bset-kbetr /  10 ).
**    l_str_final-cgst_
**    ENDIF.
*    endif.
*    READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JISG' BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-sgst_amount  = l_str_bset-fwste.
*    l_str_final-ut_itc_amt = l_str_final-sgst_amount.
*      l_str_final-sgst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
*     READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JISN' BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-sgst_amount  = l_str_bset-fwste.
*     l_str_final-sgst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
*     READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JICN' BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-cgst_amount  = l_str_bset-fwste.
*      l_str_final-cgst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
**    READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JISN' BINARY SEARCH.
**  IF SY-SUBRC = 0.
**    l_str_final-tax_value = l_str_bset-fwbas.
**    l_str_final-sgst_amount  = l_str_bset-fwste.
**     l_str_final-sgst_rate =  ( l_str_bset-kbetr /  10 ).
**    ENDIF.
*     READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JIIN'  BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-igst_amount  = l_str_bset-fwste.
*     l_str_final-igst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
*    READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JISG' BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-sgst_amount  = l_str_bset-fwste.
*    l_str_final-ut_itc_amt = l_str_final-sgst_amount .
*     l_str_final-sgst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
*      READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JICR' BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-cgst_amount  = l_str_bset-fwste.
*    l_str_final-cent_itc_amt = l_str_final-cgst_amount.
*     l_str_final-cgst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
*     READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JOCG' BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-cgst_amount  = l_str_bset-fwste.
*    l_str_final-cent_itc_amt = l_str_final-cgst_amount.
*     l_str_final-cgst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
**         WHEN 'JIIG' OR 'JIIR' OR 'JOIG' .
*              READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JIIG' BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-igst_amount  = l_str_bset-fwste.
*    l_str_final-intr_itc_amt = l_str_final-igst_amount.
*     l_str_final-igst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
*      READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JOIG' BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-igst_amount  = l_str_bset-fwste.
*    l_str_final-intr_itc_amt = l_str_final-igst_amount.
*      l_str_final-igst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
*      READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JIIR' BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-igst_amount  = l_str_bset-fwste.
*    l_str_final-intr_itc_amt = l_str_final-igst_amount.
*      l_str_final-igst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
**    READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr BINARY SEARCH.
**    if sy-SUBRC = 0.
*    READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JISG' BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-sgst_amount  = l_str_bset-fwste.
*    l_str_final-ut_itc_amt = l_str_final-sgst_amount.
*      l_str_final-sgst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
**    endif.
*      READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JISR' BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-sgst_amount  = l_str_bset-fwste.
*    l_str_final-ut_itc_amt = l_str_final-sgst_amount.
*     l_str_final-sgst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
*      READ TABLE l_tab_bset into l_str_bset with key bukrs = l_Str_bseg-bukrs belnr = l_str_bseg-belnr gjahr = l_Str_bseg-gjahr kschl = 'JOSG'BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    l_str_final-tax_value = l_str_bset-fwbas.
*    l_str_final-sgst_amount  = l_str_bset-fwste.
*    l_str_final-ut_itc_amt = l_str_final-sgst_amount.
*     l_str_final-sgst_rate =  ( l_str_bset-kbetr /  10 ).
*    ENDIF.
*    if l_str_final-igst_amount is INITIAL.
*      CLEAR l_str_final-intr_itc_amt.
*      endif.
    DELETE l_tab_bset WHERE mwskz = 'V0'.

    READ TABLE l_tab_bset TRANSPORTING NO FIELDS WITH KEY bukrs = l_str_bkpf-bukrs
                                                           belnr = l_str_bkpf-belnr
                                                           gjahr = l_str_bkpf-gjahr BINARY SEARCH.
    IF sy-subrc = 0.
      l_var_tabix = sy-tabix.

      LOOP AT l_tab_bset INTO l_str_bset FROM l_var_tabix WHERE bukrs = l_str_bkpf-bukrs   AND
                                                                belnr = l_str_bkpf-belnr AND
                                                                gjahr = l_str_bkpf-gjahr.
*  READ TABLE l_tab_bset INTO l_str_bset with KEY bukrs = l_str_bseg-bukrs
*                                                          belnr = l_str_bseg-belnr
*                                                          gjahr = l_str_bseg-gjahr
*                                                          kschl = 'JICG'  .
        CASE l_str_bset-kschl.
          WHEN  'JICN' OR 'JOCG'  .
            l_str_final-tax_value =  l_str_bset-fwbas.
            IF l_str_bset-shkzg = 'S'.
              l_str_final-cgst_amount =  l_str_bset-fwste. " l_str_final-cgst_amount +
            ELSE.
              l_str_final-cgst_amount =  l_str_bset-fwste * -1. "l_str_final-cgst_amount +
            ENDIF.
            IF  l_str_final-cgst_amount < 0 .
              l_str_final-cgst_amount =  l_str_final-cgst_amount * -1.
            ENDIF.
*               if l_str_final-cgst_rate is INITIAL.
            l_str_final-cgst_rate =  ( l_str_bset-kbetr /  10 ).
            l_str_final-sub_supply_type = 'SUP'.
            l_str_final-document_type = 'INV'.
*               endif.
          WHEN  'JIIN' OR 'JOIG' .
            l_str_final-tax_value =  l_str_bset-fwbas.
            IF l_str_bset-shkzg = 'S'.
              l_str_final-igst_amount = l_str_bset-fwste. " l_str_final-igst_amount +
            ELSE.
              l_str_final-igst_amount =  l_str_bset-fwste * -1. " l_str_final-igst_Amount -
            ENDIF.
            IF  l_str_final-igst_amount < 0 .
              l_str_final-igst_amount =  l_str_final-igst_amount * -1.
            ENDIF.
*               if l_str_final-igst_rate is INITIAL.
            l_str_final-igst_rate =  ( l_str_bset-kbetr /  10 ).
            l_str_final-sub_supply_type = 'SUP'.
            l_str_final-document_type = 'INV'.
*              endif.
*
          WHEN  'JISN' OR 'JOSG' .
            l_str_final-tax_value =  l_str_bset-fwbas.
            IF l_str_bset-shkzg = 'S'.
              l_str_final-sgst_amount =  l_str_bset-fwste. " l_str_final-sgst_amount +
            ELSE.
              l_str_final-sgst_amount =   l_str_bset-fwste * -1. " l_str_final-sgst_amount +
            ENDIF.
            IF  l_str_final-sgst_amount < 0 .
              l_str_final-sgst_amount =  l_str_final-sgst_amount * -1.
            ENDIF.
*               if l_str_final-sgst_rate is INITIAL.
            l_str_final-sgst_rate =   ( l_str_bset-kbetr /  10 ).
            l_str_final-sub_supply_type = 'SUP'.
            l_str_final-document_type = 'INV'.
*               ENDIF.
          WHEN 'JICG'  .  "OR 'JICR' 'JOCG'
            l_str_final-tax_value = l_str_bset-fwbas.
            IF l_str_bset-shkzg = 'S'.
              l_str_final-cent_itc_amt = l_str_bset-fwste. " l_str_final-cent_itc_amt +
              l_str_final-elig_itc = 'is'.
              l_str_final-cgst_amount = l_str_final-cent_itc_amt.
            ELSE.
              l_str_final-cent_itc_amt =  l_str_bset-fwste * -1. "l_str_final-cent_itc_amt -
              l_str_final-tax_value = l_str_final-tax_value * -1.
*                 IF  l_str_final-cent_tax_itc_amt < 0 .
*                   l_str_final-cent_tax_itc_amt =  l_str_final-cent_tax_itc_amt * -1.
*                 ENDIF.
              l_str_final-cgst_amount = l_str_final-cent_itc_amt.

            ENDIF.
            IF l_str_final-cgst_rate IS INITIAL.
              l_str_final-cgst_rate =  ( l_str_bset-kbetr /  10 ).
            ENDIF.
            l_str_final-sub_supply_type = 'SUP'.
            l_str_final-document_type = 'INV'.
          WHEN 'JIIG'  ."'JIIR' OR 'JOIG'

            l_str_final-tax_value = l_str_bset-fwbas.
            IF l_str_bset-shkzg = 'S'.
              l_str_final-intr_itc_amt = l_str_bset-fwste. " l_str_final-intr_itc_amt +
              l_str_final-elig_itc = 'is'.
              l_str_final-igst_amount = l_str_final-intr_itc_amt.
            ELSE.
              l_str_final-intr_itc_amt =  l_str_bset-fwste * -1. "  l_str_final-intr_itc_amt +
              l_str_final-tax_value = l_str_final-tax_value * -1.
*                 IF  l_str_final-cent_tax_itc_amt < 0 .
*                   l_str_final-cent_tax_itc_amt =  l_str_final-cent_tax_itc_amt * -1.
*                 ENDIF.
              l_str_final-igst_amount = l_str_final-intr_itc_amt.

            ENDIF.
            IF l_str_final-igst_rate IS INITIAL.
              l_str_final-igst_rate =  ( l_str_bset-kbetr /  10 ).
            ENDIF.
            l_str_final-sub_supply_type = 'SUP'.
            l_str_final-document_type = 'INV'.
          WHEN 'JIMD'  ."'JIIR' OR
            l_str_final-tax_value = l_str_bset-fwbas.
            IF l_str_bset-shkzg = 'S'.
*                 l_str_final-intr_itc_amt = l_str_bset-fwste. " l_str_final-intr_itc_amt +
              l_str_final-igst_amount = l_str_bset-fwste.
              l_str_final-elig_itc = 'ip'.
            ELSE.
              l_str_final-igst_amount =  l_str_bset-fwste * -1. "  l_str_final-intr_itc_amt +
              l_str_final-tax_value = l_str_final-tax_value * -1.
*                 IF  l_str_final-cent_tax_itc_amt < 0 .
*                   l_str_final-cent_tax_itc_amt =  l_str_final-cent_tax_itc_amt * -1.
            ENDIF.
*                 l_str_final-igst_amount = l_str_final-intr_itc_amt.
            l_str_final-igst_rate =   ( l_str_bset-kbetr /  10 ).
            l_str_final-sub_supply_type = 'IMPG'.


*               ENDIF.


*               if l_str_final-igst_rate is INITIAL.
*                l_str_final-igst_rate =  ( l_str_bset-kbetr /  10 ).
*                endif.
          WHEN 'JISG'  ."'JISR' OR 'JOSG'
            l_str_final-tax_value = l_str_bset-fwbas.
            IF l_str_bset-shkzg = 'S'.
              l_str_final-ut_itc_amt =  l_str_bset-fwste. " l_str_final-ut_itc_amt +
              l_str_final-elig_itc = 'is'.
              l_str_final-sgst_amount = l_str_final-ut_itc_amt.
            ELSE.
              l_str_final-ut_itc_amt = l_str_bset-fwste * -1. " l_str_final-ut_itc_amt -
              l_str_final-tax_value = l_str_final-tax_value * -1.
*                 IF  l_str_final-cent_tax_itc_amt < 0 .
*                   l_str_final-cent_tax_itc_amt =  l_str_final-cent_tax_itc_amt * -1.
*                 ENDIF.
              l_str_final-sgst_amount = l_str_final-ut_itc_amt.

            ENDIF.
*               if l_str_final-sgst_rate  is INITIAL.
            l_str_final-sgst_rate =   ( l_str_bset-kbetr /  10 ).
            l_str_final-sub_supply_type = 'SUP'.
            l_str_final-document_type = 'INV'.
*              endif.
        ENDCASE.

*    if l_str_final-cent_itc_amt is NOT INITIAL and l_str_final-ut_itc_amt is NOT INITIAL.
*      l_str_final-ELIG_ITC = 'is'.
**      endif.
*      elseif l_str_final-intr_itc_amt is NOT INITIAL.
*        l_str_final-ELIG_ITC = 'is'.
**        ENDIF.
*        else.
        IF l_str_final-elig_itc IS INITIAL.

          IF l_str_bset-kschl NE 'JIIG' AND l_str_bset-kschl NE 'JICG' AND l_str_bset-kschl NE 'JISG'.
            l_str_final-elig_itc = 'no'.
          ENDIF.
        ENDIF.


        READ TABLE l_tab_bseg_hsn INTO l_str_bseg_hsn WITH KEY bukrs = l_str_bkpf-bukrs
                                                           belnr = l_str_bkpf-belnr
                                                           gjahr  = l_str_bkpf-gjahr
                                                           buzei = l_str_bset-buzei.
        IF sy-subrc = 0.
          l_str_final-hsn = l_str_bseg_hsn-hsn_sac.
        ENDIF.
*  endloop.
        CLEAR : l_str_bseg_matnr , l_str_final-product_name.

        READ TABLE l_tab_bseg_matnr  INTO l_str_bseg_matnr WITH KEY bukrs = l_str_bkpf-bukrs
                                                          belnr = l_str_bkpf-belnr
                                                          gjahr = l_str_bkpf-gjahr
                                                          txgrp = l_str_bset-txgrp BINARY SEARCH.

        IF sy-subrc = 0.
          l_str_final-product_name = l_str_bseg_matnr-matnr.
        ENDIF.
        CLEAR : l_str_makt ,l_str_final-description.
        READ TABLE l_tab_makt INTO l_str_makt WITH KEY matnr = l_str_bseg_matnr-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          l_str_final-description = l_str_makt-maktx.

        ENDIF.
        CLEAR  : l_str_bseg_menge , l_str_final-quantity.

        READ TABLE l_tab_bseg_menge INTO l_str_bseg_menge WITH KEY bukrs = l_str_bkpf-bukrs
                                                         belnr = l_str_bkpf-belnr
                                                         gjahr  = l_str_bkpf-gjahr
                                                         txgrp = l_str_bset-txgrp .
        IF sy-subrc  = 0.
          l_str_final-quantity = l_str_bseg_menge-menge.
*  l_str_final-uom = l_str_bseg_menge-meins.

          CLEAR l_str_zei_uom_maping.
          CLEAR l_str_final-uom.
          READ TABLE l_tab_zei_uom_maping INTO l_str_zei_uom_maping WITH KEY zsap_uom = l_str_bseg_menge-meins.
          IF sy-subrc = 0.
            l_str_final-uom = l_str_zei_uom_maping-zgsp_uom.
          ENDIF.
        ENDIF.
*
        IF l_str_final-cgst_amount IS NOT INITIAL AND l_str_final-sgst_amount IS NOT INITIAL.
          CLEAR l_str_final-igst_amount.
        ENDIF.
        IF l_str_final-cgst_rate IS NOT INITIAL AND l_str_final-sgst_rate IS NOT INITIAL.
          CLEAR l_str_final-igst_rate.
        ENDIF.
        l_str_final-custom3 = l_str_bset-txgrp.
        l_str_final-custom6 = l_str_bkpf-budat.
        IF l_str_final-tax_value < 1.
          l_str_final-tax_value = l_str_final-tax_value * -1.
        ENDIF.
        IF l_str_final-igst_amount < 1.
          l_str_final-igst_amount = l_str_final-igst_amount * -1.
        ENDIF.
        IF l_str_final-cgst_amount < 1.
          l_str_final-cgst_amount = l_str_final-cgst_amount * -1.
        ENDIF.
        IF l_str_final-cent_itc_amt < 1.
          l_str_final-cent_itc_amt = l_str_final-cent_itc_amt * -1.
        ENDIF.
        IF l_str_final-ut_itc_amt < 1.
          l_str_final-ut_itc_amt = l_str_final-ut_itc_amt * -1.
        ENDIF.
        IF l_str_final-intr_itc_amt < 1.
          l_str_final-intr_itc_amt = l_str_final-intr_itc_amt * -1.
        ENDIF.
        IF l_str_final-sgst_amount < 1.
          l_str_final-sgst_amount = l_str_final-sgst_amount * -1.
        ENDIF..
        IF l_str_final-cgst_rate < 1.
          l_str_final-cgst_rate = l_str_final-cgst_rate  * -1.
        ENDIF.
        IF l_str_final-sgst_rate < 1.
          l_str_final-sgst_rate = l_str_final-sgst_rate  * -1.
        ENDIF.
        IF l_str_final-igst_rate < 1.
          l_str_final-igst_rate = l_str_final-igst_rate  * -1.
        ENDIF.
*
        l_str_final-document_value = l_str_final-tax_value + l_str_final-cgst_amount + l_str_final-igst_amount + l_str_final-sgst_amount.
        IF l_str_final-elig_itc = 'no'.
          CLEAR : l_str_final-intr_itc_amt , l_str_final-cent_itc_amt , l_str_final-ut_itc_amt.
        ENDIF.

        IF l_str_final-sub_supply_type = 'IMPG'.
          l_str_final-document_type = 'BOE'.
        ENDIF.

        IF l_str_bset-kschl = 'JOCG' OR l_str_bset-kschl = 'JOIG' OR l_str_bset-kschl = 'JOSG'.
          l_str_final-rev_charges = 'Y'.
        ELSE.
          l_str_final-rev_charges  = 'N'.
        ENDIF.

*      IF ( l_str_final-cgst_rate IS INITIAL AND l_str_final-sgst_rate IS INITIAL ) or l_str_final-igst_rate IS INITIAL.
*        l_str_final-CUSTOM5 = 'X'.
*        ENDIF.

        IF l_str_final-elig_itc = 'ip' OR l_str_final-elig_itc = 'is'.
          l_str_final-cent_itc_amt = l_str_final-cgst_amount.
          l_str_final-ut_itc_amt = l_str_final-sgst_amount.
          l_str_final-intr_itc_amt = l_str_final-igst_amount.
        ENDIF.

        AT END OF txgrp.
*
*l_str_final-sgst_amount = l_str_final-cgst_amount.
*l_str_final-sgst_rate = l_str_final-cgst_rate.

          APPEND l_str_final TO gt_final_inward.
*    ENDLOOP.
**    CLEAR : l_str_final.
          CLEAR : l_str_final-cgst_rate , l_str_final-igst_rate , l_str_final-sgst_rate ,l_str_final-cgst_amount , l_str_final-sgst_amount , l_str_final-igst_amount ,l_str_final-cent_itc_amt ,
          l_str_final-ut_itc_amt , l_str_final-intr_itc_amt ,l_str_final-rev_charges.
        ENDAT.
*    endif.
*    CLEAR : l_str_final.
      ENDLOOP.
    ENDIF.


  ENDLOOP.
*  MOVE-CORRESPONDING GT_FINAL_INWARD   TO GT_FINAL_INWARD1.
  DELETE gt_final_inward WHERE custom2 NOT IN  s_bupla.
  SORT gt_final_inward BY    custom1 custom2 custom3.
*   SORT GT_FINAL_INWARD BY mandt document_no document_date.
*   delete ADJACENT DUPLICATES FROM GT_FINAL_INWARD COMPARING custom1.
  MOVE gt_final_inward TO gt_final_inward1.
  CLEAR gt_final_inward.
  CLEAR gt_final_inward.
  SORT gt_final_inward1 BY location_gstin custom1 hsn uom.
  LOOP AT gt_final_inward1 INTO gl_final_inward1.
    l_str_final-custom1 = gl_final_inward1-custom1.
    l_str_final-product_name = gl_final_inward1-product_name.
    l_str_final-hsn = gl_final_inward1-hsn.
    l_str_final-uom = gl_final_inward1-uom.
    l_str_final-product_name = gl_final_inward1-product_name.
    l_str_final-description = gl_final_inward1-description.
    l_str_final-custom2 = gl_final_inward1-custom2.
    l_str_final-custom3 = gl_final_inward1-custom3.
    l_str_final-document_no = gl_final_inward1-document_no.
    l_str_final-document_date = gl_final_inward1-document_date.
    l_str_final-location_gstin = gl_final_inward1-location_gstin.
    l_str_final-location_legal_name = gl_final_inward1-location_legal_name.
    l_str_final-document_purpose = gl_final_inward1-document_purpose.
    l_str_final-supply_type = gl_final_inward1-supply_type.
    l_str_final-sub_supply_type = gl_final_inward1-sub_supply_type.
    l_str_final-sub_supply_description = gl_final_inward1-sub_supply_description.
    l_str_final-document_type = gl_final_inward1-document_type.
    l_str_final-series_code = gl_final_inward1-series_code.
    l_str_final-reference_doc_type = gl_final_inward1-reference_doc_type.
    l_str_final-reference_doc_no = gl_final_inward1-reference_doc_no.
    l_str_final-reference_doc_date = gl_final_inward1-reference_doc_date.

    l_str_final-transaction_type = gl_final_inward1-transaction_type.
    l_str_final-pos = gl_final_inward1-pos.
    l_str_final-bill_from_gstin = gl_final_inward1-bill_from_gstin.

    l_str_final-bill_from_oth_party_name = gl_final_inward1-bill_from_oth_party_name.
    l_str_final-bill_from_address1 = gl_final_inward1-bill_from_address1.
    l_str_final-bill_from_address2  = gl_final_inward1-bill_from_address2.
    l_str_final-bill_from_city = gl_final_inward1-bill_from_city.
    l_str_final-bill_from_state = gl_final_inward1-bill_from_state.
    l_str_final-bill_from_post_code = gl_final_inward1-bill_from_post_code.

    l_str_final-dispatch_from_gstin = gl_final_inward1-dispatch_from_gstin.
    l_str_final-dispatch_from_oth_party_name = gl_final_inward1-dispatch_from_oth_party_name.

    l_str_final-dispatch_from_address1 = gl_final_inward1-dispatch_from_address1.

    l_str_final-dispatch_from_address2  = gl_final_inward1-dispatch_from_address2.
    l_str_final-dispatch_from_city = gl_final_inward1-dispatch_from_city.
    l_str_final-dispatch_from_state = gl_final_inward1-dispatch_from_state.
    l_str_final-dispatch_from_post_code = gl_final_inward1-dispatch_from_post_code.
    l_str_final-ctin = gl_final_inward1-ctin.
    l_str_final-email = gl_final_inward1-email.
    l_str_final-mobile = gl_final_inward1-mobile.
    l_str_final-bill_to_gstin = gl_final_inward1-bill_to_gstin.
    l_str_final-bill_to_name = gl_final_inward1-bill_to_name.
    l_str_final-bill_to_address1 = gl_final_inward1-bill_to_address1.
    l_str_final-bill_to_address2 = gl_final_inward1-bill_to_address2.
    l_str_final-bill_to_state  = gl_final_inward1-bill_to_state.
    l_str_final-bill_to_city = gl_final_inward1-bill_to_city.
    l_str_final-bill_to_post_code = gl_final_inward1-bill_to_post_code.
    l_str_final-ship_to_gstin = gl_final_inward1-ship_to_gstin.
    l_str_final-ship_to_name = gl_final_inward1-ship_to_name.
    l_str_final-ship_to_address1 = gl_final_inward1-ship_to_address1.
    l_str_final-ship_to_address2 = gl_final_inward1-ship_to_address2.
    l_str_final-quantity = l_str_final-quantity + gl_final_inward1-quantity.
    l_str_final-tax_value = l_str_final-tax_value + gl_final_inward1-tax_value.
    l_str_final-cgst_amount = l_str_final-cgst_amount + gl_final_inward1-cgst_amount.
    l_str_final-igst_amount = l_str_final-igst_amount + gl_final_inward1-igst_amount.
    l_str_final-sgst_amount = l_str_final-sgst_amount + gl_final_inward1-sgst_amount.
    l_str_final-cgst_rate =   gl_final_inward1-cgst_rate.
    l_str_final-sgst_rate = gl_final_inward1-sgst_rate.
    l_str_final-igst_rate =   gl_final_inward1-igst_rate.
    l_str_final-document_value = l_str_final-document_value + gl_final_inward1-document_value.
    l_str_final-elig_itc  = gl_final_inward1-elig_itc.
    l_str_final-rev_charges = gl_final_inward1-rev_charges.


*P_TO_STATE
*SHIP_TO_CITY
    AT END OF hsn.
      APPEND l_str_final TO gt_final_inward.
      CLEAR l_str_final.
    ENDAT.

  ENDLOOP.

ENDFORM.

FORM top_of_page .

*TITLE
  CLEAR: lt_header,ls_header.

  ls_header-typ = 'H'.
  ls_header-info = 'GSTR2 Generator'.
  APPEND ls_header TO lt_header.
  CLEAR ls_header.

*DATE
  ls_header-typ = 'S'.
  CONCATENATE 'Date: ' ' '  s_date-low+6(2) '.' s_date-low+4(2) '.' s_date-low(4) ' ' 'To' ' '  s_date-high+6(2) '.' s_date-high+4(2) '.' s_date-high(4)
              INTO ls_header-info.
  APPEND ls_header TO lt_header.
  CLEAR: ls_header.

*TOTAL NO. OF RECORDS SELECTED
  DESCRIBE TABLE gt_final_inward LINES l_var_cnt.
  ld_linesc = l_var_cnt.
  ls_header-typ = 'S'.
  CONCATENATE 'Number of records found : ' ld_linesc  INTO ls_header-info.

*ls_HEADER-INFO = T_LINE.
  APPEND ls_header TO lt_header.
  CLEAR: ls_header, t_line.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header.
ENDFORM.                    " TOP_OF_PAGE
