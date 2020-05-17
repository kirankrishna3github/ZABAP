*&---------------------------------------------------------------------*
*& Include          ZGSP_GL_GENRATOR_MM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZGSP_GL_GENRATOR_MM
*&---------------------------------------------------------------------*
************************************************************************
****Start-of-selection.
***START-OF-SELECTION.
***
***  PERFORM DATA_RETRIEVAL.
***  PERFORM BUILD_FIELDCATALOG.
***  PERFORM BUILD_LAYOUT.
***  PERFORM DISPLAY_ALV_REPORT.
***  PERFORM GENERATE_FILE.

*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM data_retrieval.
***  SELECT * FROM BKPF INTO TABLE IT_BKPF WHERE BELNR  IN S_BELNR
***                                         AND  BUKRS  IN S_BUKRS
***                                         AND  GJAHR  IN S_GJAHR
***                                         AND  BUDAT  IN S_DATE.



    SELECT bukrs
           belnr
           gjahr
           blart
           bldat
           budat
           monat
           xreversal
           xblnr_alt
           stblg
           xblnr
           tcode   FROM bkpf INTO TABLE it_bkpf WHERE bukrs  IN s_bukrs
                                         AND  gjahr  IN s_gjahr
                                         AND  budat  IN s_date.
*                                         AND  BELNR  IN S_BELNR.

    IF it_bkpf IS NOT INITIAL.
      SELECT bukrs
             belnr
             gjahr
             buzei
             txgrp
             hkont
             kunnr
             lifnr
             plc_sup
             hsn_sac
             bupla
             buzid
             koart
             dmbtr
             shkzg
             zuonr
             sgtxt
             bschl
             pswsl FROM bseg INTO TABLE it_bseg
                  FOR ALL ENTRIES IN  it_bkpf    WHERE bukrs =  it_bkpf-bukrs
                                       AND   belnr = it_bkpf-belnr
                                       AND   gjahr  = it_bkpf-gjahr
                                     AND   bupla   IN  s_bupla .
      SELECT bukrs
             belnr
             gjahr
             hkont
             txgrp
             buzei
             kunnr
             lifnr
             plc_sup
             hsn_sac
             bupla
             buzid
             koart
             dmbtr
             shkzg
             zuonr
             sgtxt
             bschl
             pswsl  augbl FROM bseg INTO TABLE it_bseg_tmp
                  FOR ALL ENTRIES IN  it_bkpf    WHERE bukrs =  it_bkpf-bukrs
                                       AND   belnr = it_bkpf-belnr
                                       AND   gjahr  = it_bkpf-gjahr
                                     AND   bupla   IN  s_bupla .

      SELECT bukrs
            belnr
            gjahr
            buzei
            hkont
            shkzg
            hwste
            txgrp
            hwbas
         FROM bset INTO TABLE lt_bset
        FOR ALL ENTRIES IN it_bseg
        WHERE bukrs =  it_bseg-bukrs
        AND   belnr = it_bseg-belnr
        AND   gjahr  = it_bseg-gjahr.

      IF it_bseg IS NOT INITIAL.
        SELECT * FROM kna1 INTO TABLE lt_kna1
          FOR ALL ENTRIES IN it_bseg WHERE kunnr = it_bseg-kunnr.

        SELECT * FROM lfa1 INTO TABLE lt_lfa1
          FOR ALL ENTRIES IN it_bseg WHERE lifnr = it_bseg-lifnr.

        SELECT * FROM j_1bbranch INTO TABLE it_j_1bbranch
          FOR ALL ENTRIES IN it_bseg WHERE bukrs =  it_bseg-bukrs
                                           AND   branch  = it_bseg-bupla.
      ENDIF.

      SELECT * FROM  skat INTO TABLE lt_tab_skat
                          FOR ALL ENTRIES IN  it_bseg
                          WHERE saknr =  it_bseg-hkont
                          AND   spras = 'EN'.
    ENDIF.


    IF it_bseg IS NOT INITIAL.
*      IT_BSEG_TMP = IT_BSEG.
      it_bseg_tmp_d = it_bseg.
      it_bseg_tmp_k = it_bseg.
      it_bseg_tmp_l = it_bseg.
      it_bseg_tmp_pos = it_bseg.

      DELETE it_bseg_tmp WHERE buzid IS NOT INITIAL."""KS
      DELETE it_bseg_tmp_d WHERE koart <> 'D'.
      DELETE it_bseg_tmp_k WHERE koart <> 'K'.
      DELETE it_bseg_tmp_l WHERE bupla IS INITIAL.
      DELETE it_bseg_tmp_pos WHERE plc_sup IS INITIAL.
    ENDIF.
    SORT it_bseg_tmp BY bukrs belnr gjahr hkont txgrp.
    DELETE ADJACENT DUPLICATES FROM it_bseg_tmp COMPARING bukrs belnr gjahr hkont txgrp  ."""KS New

    SORT it_bkpf BY bukrs belnr gjahr.
    SORT it_j_1bbranch BY bukrs branch.
    SORT lt_kna1 BY kunnr.
    SORT lt_lfa1 BY lifnr.
    SORT lt_bset BY bukrs  belnr  gjahr  txgrp hkont.
    SORT it_bseg_tmp BY bukrs  belnr  gjahr  buzei.
    SORT it_bseg_tmp_l BY bukrs  belnr  .
    SORT it_bseg_tmp_pos BY bukrs  belnr  .
    SORT lt_tab_skat    BY saknr.

    LOOP AT it_bseg INTO wa_bseg.
      READ TABLE it_bkpf INTO wa_bkpf WITH  KEY bukrs =  wa_bseg-bukrs  belnr = wa_bseg-belnr  gjahr  = wa_bseg-gjahr BINARY SEARCH.
      IF sy-subrc = 0.
        IF wa_bkpf-xreversal IS NOT INITIAL.
          CLEAR : wa_final, wa_bkpf, wa_bseg.
          CONTINUE.
        ENDIF.
        IF wa_bkpf-stblg IS NOT INITIAL.
          CLEAR : wa_final, wa_bkpf, wa_bseg.
          CONTINUE.
        ENDIF.
        wa_final-custom3 = wa_bkpf-blart.


        READ TABLE it_bseg_tmp_d INTO ls_bseg_tmp_d WITH KEY  bukrs =  wa_bseg-bukrs  belnr = wa_bseg-belnr  gjahr  = wa_bseg-gjahr." BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE it_j_1bbranch INTO wa_j_1bbranch WITH KEY bukrs = ls_bseg_tmp_d-bukrs  branch = ls_bseg_tmp_d-bupla BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-location_gstin     = wa_j_1bbranch-gstin .
            wa_final-business_place     = wa_j_1bbranch-branch .
          ENDIF.
          READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_bseg_tmp_d-kunnr BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-customer_code = ls_kna1-kunnr.
            wa_final-customer_name = ls_kna1-name1.
            wa_final-customer_gst = ls_kna1-stcd3.
          ENDIF.
        ENDIF.

        READ TABLE it_bseg_tmp_k INTO ls_bseg_tmp_k WITH KEY  bukrs =  wa_bseg-bukrs  belnr = wa_bseg-belnr  gjahr  = wa_bseg-gjahr." BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE it_j_1bbranch INTO wa_j_1bbranch WITH KEY bukrs = ls_bseg_tmp_k-bukrs  branch = ls_bseg_tmp_k-bupla BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-location_gstin     = wa_j_1bbranch-gstin .
            wa_final-business_place     = wa_j_1bbranch-branch .
          ENDIF.
          READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_bseg_tmp_k-lifnr BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-customer_code = ls_lfa1-lifnr.
            wa_final-customer_name = ls_lfa1-name1.
            wa_final-customer_gst = ls_lfa1-stcd3.
          ENDIF.
        ENDIF.

*        IF WA_BSEG-BUZID <> 'T'.
*          READ TABLE IT_BSEG_TMP INTO LS_BSEG_TMP WITH  KEY BUKRS =  WA_BSEG-BUKRS
*                                                            BELNR = WA_BSEG-BELNR
*                                                            GJAHR  = WA_BSEG-GJAHR
*                                                            BUZEI  = WA_BSEG-BUZEI BINARY SEARCH.
*          IF SY-SUBRC = 0.
*            IF   LS_BSEG_TMP-KOART = 'S'. "For txable value .
**              WA_FINAL-AMOUNT             = LS_BSEG_TMP-DMBTR.
**              WA_FINAL-CUSTOM6            = LS_BSEG_TMP-KOART.
*                READ TABLE LT_BSET INTO LS_BSET_TEMP WITH  KEY BUKRS = WA_BSEG-BUKRS
*                                                          BELNR = WA_BSEG-BELNR
*                                                          GJAHR = WA_BSEG-GJAHR
*                                                           TXGRP = WA_BSEG-TXGRP BINARY SEARCH .
*                IF SY-SUBRC = 0.
*                  WA_FINAL-AMOUNT             = LS_BSET_TEMP-HWBAS.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*        ELSE.
***********************************************Logic for Taxable value
*
*          READ TABLE LT_BSET INTO LS_BSET WITH  KEY BUKRS = WA_BSEG-BUKRS
*                                                    BELNR = WA_BSEG-BELNR
*                                                    GJAHR = WA_BSEG-GJAHR
*                                                     TXGRP = WA_BSEG-TXGRP
*                                                    HKONT = WA_BSEG-HKONT BINARY SEARCH .
*          IF SY-SUBRC = 0.
*            WA_FINAL-AMOUNT             = LS_BSET-HWSTE.
*            WA_FINAL-CUSTOM6            = WA_BSEG-KOART.
*            WA_FINAL-CUSTOM7            = WA_BSEG-BUZID.
*          ENDIF.
*        ENDIF.
**********************************************************************


        READ TABLE it_bseg_tmp_l INTO ls_bseg_tmp_l WITH  KEY bukrs = wa_bseg-bukrs  belnr = wa_bseg-belnr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-business_place     = ls_bseg_tmp_l-bupla .
          READ TABLE it_j_1bbranch INTO wa_j_1bbranch WITH KEY bukrs = ls_bseg_tmp_l-bukrs  branch = ls_bseg_tmp_l-bupla BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-location_gstin     = wa_j_1bbranch-gstin .
          ENDIF.
        ENDIF.


        READ TABLE it_bseg_tmp_pos INTO ls_bseg_tmp_pos WITH  KEY bukrs = wa_bseg-bukrs  belnr = wa_bseg-belnr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-place_supply       =  ls_bseg_tmp_pos-plc_sup .
        ENDIF.

        IF wa_bseg-shkzg = 'H'.
          wa_final-amount             = wa_bseg-dmbtr * ( -1 ).
        ELSE.
          wa_final-amount             = wa_bseg-dmbtr.
        ENDIF.
        wa_final-account_type       = wa_bseg-koart.
        wa_final-gl_account         = wa_bseg-hkont.
*      WA_FINAL-DOCUMENT_TYPE      = WA_BKPF-BLART.
        wa_final-document_type      = 'INV' .
        wa_final-document_no        = wa_bkpf-belnr .
        wa_final-document_date      = wa_bkpf-bldat .
        IF wa_final-place_supply IS INITIAL.
          wa_final-place_supply       = wa_bseg-plc_sup .
        ENDIF.
        wa_final-debit_credit       = wa_bseg-shkzg .
        wa_final-reference          = wa_bkpf-xblnr .
        wa_final-assignment         = wa_bseg-zuonr   .
        wa_final-text               = wa_bseg-sgtxt .
        wa_final-postingkey         = wa_bseg-bschl .
        wa_final-posting_date       = wa_bkpf-budat .
        wa_final-currency           = wa_bseg-pswsl .
*        WA_FINAL-CUSTOMER_CODE      = WA_BSEG-LIFNR .
*      WA_FINAL-CUSTOMER_GST       = WA_LFA1-STCD3 .
*      WA_FINAL-CUSTOMER_NAME      = WA_LFA1-NAME1 .
        wa_final-tcode              = wa_bkpf-tcode .
        wa_final-hsn                = wa_bseg-hsn_sac .
        wa_final-custom1            = wa_bseg-buzei .
        wa_final-custom2            = wa_bseg-txgrp .
*      WA_FINAL-CUSTOM3            = WA_BSEG- .
*      WA_FINAL-CUSTOM4            = WA_BSEG- .
*      WA_FINAL-CUSTOM5            = WA_BSEG- .
*      WA_FINAL-CUSTOM6            = WA_BSEG- .
*      WA_FINAL-CUSTOM7            = WA_BSEG- .
*      WA_FINAL-CUSTOM8            = WA_BSEG- .
*      WA_FINAL-CUSTOM9            = WA_BSEG- .
*      WA_FINAL-CUSTOM10           = WA_BSEG- .
        IF  wa_final-customer_gst = 'NA'.
          CLEAR :wa_final-customer_gst .
        ENDIF.
      ENDIF.
      READ TABLE lt_tab_skat INTO ls_skat WITH  KEY saknr = wa_bseg-hkont BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-gl_account_desc    = ls_skat-txt50.
      ENDIF.

*      IF WA_FINAL-REFERENCE CA '/'.
*        REPLACE ALL OCCURRENCES OF REGEX '["\|!@$%^*():;.""`~?<>+]' IN WA_FINAL-REFERENCE WITH SPACE.
*      ELSE.
      REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:][:space:]]' IN wa_final-reference WITH space.
*      ENDIF.
      REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:][:space:]]' IN wa_final-text WITH space.
      REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:][:space:]]' IN wa_final-assignment WITH space.
      REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:][:space:]]' IN wa_final-customer_name WITH space.
      REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:][:space:]]' IN wa_final-gl_account_desc WITH space.
      APPEND wa_final TO it_final.
      CLEAR:wa_final .
    ENDLOOP.

    DATA :lv_amount              TYPE bseg-hwbas.
    SORT it_final1 BY business_place document_no gl_account.

*    LOOP AT IT_FINAL1 INTO WA_FINAL_T.
*      MOVE-CORRESPONDING WA_FINAL_T TO  WA_FINAL.
*      LV_AMOUNT        =     LV_AMOUNT + WA_FINAL-AMOUNT.
*      AT END OF GL_ACCOUNT.
*        WA_FINAL-AMOUNT    =  LV_AMOUNT.
*        APPEND   WA_FINAL TO IT_FINAL.
*        CLEAR:WA_FINAL,LV_AMOUNT .
*      ENDAT.
*    ENDLOOP.


    DELETE it_final WHERE amount IS INITIAL.

  ENDFORM.                    "DATA_RETRIEVAL
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       Build Fieldcatalog for ALV Report
*----------------------------------------------------------------------*
  FORM build_fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'BUSINESS_PLACE'.
    fieldcatalog-seltext_m   = 'Business Place'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'LOCATION_GSTIN'.
    fieldcatalog-seltext_m   = 'Location GSTIN'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'ACCOUNT_TYPE'.
    fieldcatalog-seltext_m   = 'Account Type'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'GL_ACCOUNT'.
    fieldcatalog-seltext_m   = 'G/L Account'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'GL_ACCOUNT_DESC'.
    fieldcatalog-seltext_m   = 'G/L Account Desc.'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.


    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'DOCUMENT_TYPE'.
    fieldcatalog-seltext_m   = 'Document Type'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'DOCUMENT_NO'.
    fieldcatalog-seltext_m   = 'Document Number'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'DOCUMENT_DATE'.
    fieldcatalog-seltext_m   = 'Document Date'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'PLACE_SUPPLY'.
    fieldcatalog-seltext_m   = 'Place of Supply (POS)'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'DEBIT_CREDIT'.
    fieldcatalog-seltext_m   = 'Debit/Credit'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'REFERENCE'.
    fieldcatalog-seltext_m   = 'Reference'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'ASSIGNMENT'.
    fieldcatalog-seltext_m   = 'Assignment'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'TEXT'.
    fieldcatalog-seltext_m   = 'Text'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'POSTINGKEY'.
    fieldcatalog-seltext_m   = 'PostingKey'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'POSTING_DATE'.
    fieldcatalog-seltext_m   = 'Posting Date'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CURRENCY'.
    fieldcatalog-seltext_m   = 'Currency'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'AMOUNT'.
    fieldcatalog-seltext_m   = 'Amount in Local Currency'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOMER_CODE'.
    fieldcatalog-seltext_m   = 'Customer Code or Vendor Code'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOMER_GST'.
    fieldcatalog-seltext_m   = 'Customer GSTIN or Vendor GSTIN'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOMER_NAME'.
    fieldcatalog-seltext_m   = 'Customer Name Or Vendor Name'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'TCODE'.
    fieldcatalog-seltext_m   = 'TCode/Transaction Code'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'HSN'.
    fieldcatalog-seltext_m   = 'HSN/SAC Code'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOM1'.
    fieldcatalog-seltext_m   = 'CUSTOM1'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOM2'.
    fieldcatalog-seltext_m   = 'CUSTOM2'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOM3'.
    fieldcatalog-seltext_m   = 'CUSTOM3'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOM4'.
    fieldcatalog-seltext_m   = 'CUSTOM4'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOM5'.
    fieldcatalog-seltext_m   = 'CUSTOM5'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOM6'.
    fieldcatalog-seltext_m   = 'CUSTOM6'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOM7'.
    fieldcatalog-seltext_m   = 'CUSTOM7'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOM8'.
    fieldcatalog-seltext_m   = 'CUSTOM8'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOM9'.
    fieldcatalog-seltext_m   = 'CUSTOM9'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

    fieldcatalog-tabname   = 'IT_FINAL'.
    fieldcatalog-fieldname   = 'CUSTOM10'.
    fieldcatalog-seltext_m   = 'CUSTOM10'.
    APPEND fieldcatalog TO fieldcatalog.
    CLEAR  fieldcatalog.

  ENDFORM.                    " BUILD_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       Build layout for ALV grid report
*----------------------------------------------------------------------*
  FORM build_layout.
    gd_layout-no_input          = 'X'.
    gd_layout-colwidth_optimize = 'X'.
  ENDFORM.                    " BUILD_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       Display report using ALV grid
*----------------------------------------------------------------------*
  FORM display_alv_report.
    gd_repid = sy-repid.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = gd_repid
*       i_callback_top_of_page  = 'TOP-OF-PAGE' "see FORM
*       i_callback_user_command = 'USER_COMMAND'
*       i_grid_title       = outtext
        is_layout          = gd_layout
        it_fieldcat        = fieldcatalog[]
*       it_special_groups  = gd_tabgroup
*       IT_EVENTS          = GT_XEVENTS
        i_save             = 'X'
*       is_variant         = z_template
      TABLES
        t_outtab           = it_final
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDFORM.                    " DISPLAY_ALV_REPORT

*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL
*&---------------------------------------------------------------------*
*       Retrieve data form EKPO table and populate itab it_ekpo
*----------------------------------------------------------------------*
  "DATA_RETRIEVAL
  " DATA_RETRIEVAL
*&---------------------------------------------------------------------*
*&      Form  GENERATE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM generate_file .

    TYPES: BEGIN OF t_final_gl,
             business_place  TYPE char100,
             location_gstin  TYPE char100,
             account_type    TYPE char100,
             gl_account      TYPE char100,
             gl_account_desc TYPE char100,
             document_type   TYPE char100,
             document_no     TYPE char100,
             document_date   TYPE char100,
             place_supply    TYPE char100,
             debit_credit    TYPE char100,
             reference       TYPE char100,
             assignment      TYPE char100,
             text            TYPE char100,
             postingkey      TYPE char100,
             posting_date    TYPE char100,
             currency        TYPE char100,
             amount          TYPE char100,
             customer_code   TYPE char100,
             customer_gst    TYPE char100,
             customer_name   TYPE char100,
             tcode           TYPE char100,
             hsn             TYPE char100,
             custom1         TYPE char100,
             custom2         TYPE char100,
             custom3         TYPE char100,
             custom4         TYPE char100,
             custom5         TYPE char100,
             custom6         TYPE char100,
             custom7         TYPE char100,
             custom8         TYPE char100,
             custom9         TYPE char100,
             custom10        TYPE char100,
           END OF t_final_gl.

    DATA: lt_final_gl TYPE TABLE OF t_final_gl,
          ls_final_gl TYPE t_final_gl.

***Internal Tables******************************************************
    DATA : lt_data      TYPE TABLE OF text,
           lt_file_gen  TYPE truxs_t_text_data,
           lt_cust_data TYPE STANDARD TABLE OF zgsp_cust_data.
***Structure***********************************************************
*  DATA : LS_OUT_INVOICE TYPE TY_FINAL_OUTWARD,
    DATA: wa_final_gl  TYPE t_final_gl,
          ls_date      TYPE range_s_dats,
          ls_cust_data TYPE zgsp_cust_data.
***Variables************************************************************
    DATA : lv_pwd_len       TYPE i, "For finding the length of the Password, This is used when scrambling the password
           lv_handle        TYPE i, "Handle for Pointing to an already connected FTP connection,used for subsequent actions on the connected FTP session
           lv_path          TYPE string, "Path that points to the FTP User's Home Directory
           lv_tsl           TYPE timestampl, "Time Stamp
           lv_time_stamp    TYPE char21, "Time Stamp
           lv_bukrs         TYPE bukrs,
           lv_answer        TYPE char1,
           lv_date          TYPE sy-datum,
           l_var_pwd_len    TYPE i,
           l_var_handle     TYPE i,
           g_var_response   TYPE char1,
           l_tab_data       TYPE TABLE OF text,
           l_var_path(100)  TYPE c,
           lv_ship_date(10) TYPE c.

***Constant*************************************************************
    CONSTANTS : l_con_key       TYPE i VALUE 26101957. "Hardcoded Handler Key,This is always '26101957'
************************************************************************
    MESSAGE 'Temp2' TYPE 'S'.
    WRITE: 'temp2'.

    IF sy-batch = abap_false.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmation'
          text_question         = 'Do you want to create the file on FTP?'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = lv_answer.
      IF lv_answer = 2.
*      MESSAGE e004(zgsp_utility).
        MESSAGE 'Not Allowed to download' TYPE 'E'.
      ENDIF.
    ENDIF.

    IF it_final IS NOT INITIAL.

      MESSAGE 'Temp3' TYPE 'S'.
      WRITE: 'temp3'.


      SELECT SINGLE * FROM zgsp_cust_data INTO ls_cust_data WHERE file_type = '02'.

      l_var_pwd_len = strlen( ls_cust_data-ftp_passward ).
      CALL FUNCTION 'HTTP_SCRAMBLE' "For Encrypting the Password
        EXPORTING
          source      = ls_cust_data-ftp_passward
          sourcelen   = l_var_pwd_len
          key         = l_con_key
        IMPORTING
          destination = ls_cust_data-ftp_passward.

      CALL FUNCTION 'FTP_CONNECT' "For connecting to the FTP Server's user directory
        EXPORTING
          user            = ls_cust_data-ftp_user
          password        = ls_cust_data-ftp_passward
          host            = ls_cust_data-ftp_host
          rfc_destination = ls_cust_data-rfc_destination
        IMPORTING
          handle          = l_var_handle
        EXCEPTIONS
          not_connected   = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        g_var_response = 1.


        MESSAGE 'FTP not connecting' TYPE 'S'.
        WRITE: 'FTP not connecting'.

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

      DATA: lv_loc_gstin TYPE kna1-stcd3.
      SORT it_final BY business_place location_gstin.
      LOOP AT it_final INTO wa_final.
        AT NEW location_gstin.
          ls_final_gl-business_place = 'Business Place/Business Code'.
          ls_final_gl-location_gstin = 'Location GSTIN'.
          ls_final_gl-account_type     = 'Account Type'.
          ls_final_gl-gl_account       = 'G/L Account '.
          ls_final_gl-gl_account_desc  = 'G/L Account Desc.'.
          ls_final_gl-document_type    = 'Document Type'.
          ls_final_gl-document_no      = 'Document Number'.
          ls_final_gl-document_date    = 'Document Date'.
          ls_final_gl-place_supply     = 'Place of Supply (POS)'.
          ls_final_gl-debit_credit     = 'Debit/Credit'.
          ls_final_gl-reference        = 'Reference'.
          ls_final_gl-assignment       = 'Assignment'.
          ls_final_gl-text             = 'Text'.
          ls_final_gl-postingkey       = 'PostingKey'.
          ls_final_gl-posting_date     = 'Posting Date'.
          ls_final_gl-currency         = 'Currency'.
          ls_final_gl-amount           = 'Amount in Local Currency'.
          ls_final_gl-customer_code    = 'Customer Code or Vendor Code'.
          ls_final_gl-customer_gst     = 'Customer GSTIN or Vendor GSTIN'.
          ls_final_gl-customer_name    = 'Customer Name Or Vendor Name'.
          ls_final_gl-tcode            = 'TCode/Transaction Code'.
          ls_final_gl-hsn              = 'HSN/SAC Code'.
          ls_final_gl-custom1          = 'Custom Field 1 (Text / Numeric)'.
          ls_final_gl-custom2          = 'Custom Field 2 (Text / Numeric)'.
          ls_final_gl-custom3          = 'Custom Field 3 (Text / Numeric)'.
          ls_final_gl-custom4          = 'Custom Field 4 (Text / Numeric)'.
          ls_final_gl-custom5          = 'Custom Field 5 (Text / Numeric)'.
          ls_final_gl-custom6          = 'Custom Field 6 (Text / Numeric)'.
          ls_final_gl-custom7          = 'Custom Field 7 (Text / Numeric)'.
          ls_final_gl-custom8          = 'Custom Field 8 (Text / Numeric)'.
          ls_final_gl-custom9          = 'Custom Field 9 (Text / Numeric)'.
          ls_final_gl-custom10         = 'Custom Field 10 (Text / Numeric)'.

          APPEND ls_final_gl TO lt_final_gl.
          CLEAR: ls_final_gl.
        ENDAT.

        MOVE-CORRESPONDING wa_final TO ls_final_gl.


        SHIFT ls_final_gl-gl_account LEFT DELETING LEADING '0'.
        SHIFT ls_final_gl-document_no LEFT DELETING LEADING '0'.


        CLEAR lv_loc_gstin.
        lv_loc_gstin = ls_final_gl-location_gstin.

        IF wa_final-amount IS INITIAL.
          CLEAR : ls_final_gl-amount.
        ENDIF.

        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
          CHANGING
            value = ls_final_gl-amount.

        IF wa_final-document_date IS INITIAL.
          CLEAR:ls_final_gl-document_date.
        ENDIF.

        IF wa_final-posting_date IS INITIAL.
          CLEAR:ls_final_gl-posting_date.
        ENDIF.

        CONDENSE:wa_final-document_date.
        IF wa_final-document_date IS NOT INITIAL.
          CONCATENATE wa_final-document_date+6(2) '-' wa_final-document_date+4(2) '-' wa_final-document_date+0(4)
                      INTO ls_final_gl-document_date.
        ELSE.
          CLEAR : ls_final_gl-document_date.
        ENDIF.

        CONDENSE:wa_final-posting_date.
        IF wa_final-posting_date IS NOT INITIAL.
          CONCATENATE wa_final-posting_date+6(2) '-' wa_final-posting_date+4(2) '-' wa_final-posting_date+0(4)
                      INTO ls_final_gl-posting_date.
        ELSE.
          CLEAR : ls_final_gl-posting_date.
        ENDIF.

        IF ls_final_gl-document_date = '- -' OR ls_final_gl-document_date = '--'.
          CLEAR:ls_final_gl-document_date.
        ENDIF.

        IF ls_final_gl-posting_date = '- -' OR ls_final_gl-posting_date = '--'.
          CLEAR:ls_final_gl-posting_date.
        ENDIF.

        APPEND ls_final_gl TO lt_final_gl.
        CLEAR: ls_final_gl.

        AT END OF location_gstin.
          IF it_final[] IS NOT INITIAL.
*** Data convert in CSV
            CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
              EXPORTING
                i_field_seperator    = ','
              TABLES
                i_tab_sap_data       = lt_final_gl
              CHANGING
                i_tab_converted_data = lt_file_gen
              EXCEPTIONS
                conversion_failed    = 1
                OTHERS               = 2.
            IF sy-subrc = 0.
              GET TIME STAMP FIELD lv_tsl.
              lv_time_stamp = lv_tsl.
***            lv_path = 'C:\Cygnet\'.
              DATA: test TYPE string."Document_5536_08_2019_090919170404.xlsx "Time Stamp
              CONCATENATE lv_loc_gstin '_Any_GL_' s_date-low+4(2)'_' s_date-low+0(4) '_' s_date-low+6(2)  s_date-low+4(2)  s_date-low+0(4) sy-uzeit '.' lv_time_stamp+14(3) '.' ls_cust_data-ftp_extension INTO l_var_path.
*            CONCATENATE 'Document_5536_' P_DATE-LOW '.' LV_TIME_STAMP+14(3) '.' LS_CUST_DATA-FTP_EXTENSION INTO L_VAR_PATH.
              CONDENSE l_var_path.
              MESSAGE 'before file generate' TYPE 'S'.
              WRITE: 'before file generate'.

              CALL FUNCTION 'FTP_R3_TO_SERVER' "For Creating a file from SAP R3 to FTP Server
                EXPORTING
                  handle         = l_var_handle
                  fname          = l_var_path
                  character_mode = abap_true
                TABLES
                  text           = lt_file_gen "Final Internal table to be written to the text file in the FTP Server's Directory
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
                MESSAGE 'after file generate' TYPE 'S'.
                WRITE: / 'after file generate'.
                IF sy-batch = abap_false.
                  WRITE:/ l_var_path.
                ENDIF.
                FREE: lt_final_gl[], it_final[].
              ENDIF.
            ENDIF.
          ELSE.
            g_var_response = 3.
          ENDIF.
        ENDAT.

        CLEAR: wa_final.
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
    ELSE.
      g_var_response = 4.

    ENDIF.

  ENDFORM.                    " GENERATE_FILE
