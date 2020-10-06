*&---------------------------------------------------------------------*
*& Report ZWRKFLO_LOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwrkflo_log.

TYPE-POOLS :slis.

TABLES:swwwihead, ptrv_head.


TYPES :BEGIN OF ts_top,
         wi_id      TYPE swwwihead-wi_id,
         wi_type    TYPE swwwihead-wi_type,
         wi_cd      TYPE swwwihead-wi_cd,
         wi_cruser  TYPE swwwihead-wi_cruser,
         top_wi_id  TYPE swwwihead-top_wi_id,
         sname      TYPE pa0001-sname,
         kostl      TYPE pa0001-kostl,
         orgeh      TYPE pa0001-orgeh,
         wi_cruser1 TYPE string,
       END OF ts_top.

TYPES :BEGIN OF ts_top1,
         wi_id      TYPE swwwihead-wi_id,
         wi_type    TYPE swwwihead-wi_type,
         wi_cd      TYPE swwwihead-wi_cd,
         wi_cruser  TYPE swwwihead-wi_cruser,
         top_wi_id  TYPE swwwihead-top_wi_id,
         sname      TYPE pa0001-sname,
         kostl      TYPE pa0001-kostl,
         orgeh      TYPE pa0001-orgeh,
         wi_cruser1 TYPE pa0001-pernr,
       END OF ts_top1.

DATA: it_top1 TYPE TABLE OF ts_top1 , "WITH HEADER LINE,
      wa_top1 TYPE ts_top1.


DATA: it_top TYPE TABLE OF ts_top,
      wa_top TYPE ts_top.

TYPES: BEGIN OF ts_swwwihead,
         wi_id     TYPE swwwihead-wi_id,
         wi_type   TYPE swwwihead-wi_type,      "type
         wi_text   TYPE swwwihead-wi_text,
         wi_rhtext TYPE swwwihead-wi_rhtext,
         wi_stat   TYPE swwwihead-wi_stat,
         wi_cd     TYPE swwwihead-wi_cd,
         wi_ct     TYPE swwwihead-wi_ct,
         wi_aed    TYPE swwwihead-wi_aed,
         wi_aagent TYPE swwwihead-wi_aagent,
*         wi_cruser TYPE swwwihead-wi_cruser,
         top_wi_id TYPE swwwihead-top_wi_id,    "top level workflow
         top_task  TYPE swwwihead-top_task,
       END OF ts_swwwihead.



TYPES: BEGIN OF ts_ptrv_head,
         pernr TYPE pa0001-pernr,
*         reinr TYPE ptrv_head-reinr,
*         dates TYPE ptrv_head-dates,
*         datv1 TYPE ptrv_head-datv1,
*         uhrv1 TYPE ptrv_head-uhrv1,
*         sum_reimbu TYPE ptrv_head-sum_reimbu,
         sname TYPE pa0001-sname,
         kostl TYPE pa0001-kostl,
         orgeh TYPE pa0001-orgeh,
         werks TYPE pa0001-werks,
         plans TYPE pa0001-plans,
         sbmod TYPE pa0001-sbmod,
       END OF ts_ptrv_head.

TYPES: BEGIN OF ts_date,
         pernr      TYPE ptrv_head-pernr,
         reinr      TYPE ptrv_head-reinr,
         dates      TYPE ptrv_head-dates,
         datv1      TYPE ptrv_head-datv1,
         uhrv1      TYPE ptrv_head-uhrv1,
         datb1      TYPE ptrv_head-datb1,
  END OF ts_date.

  DATA: it_date TYPE TABLE OF ts_date,
        wa_date TYPE ts_date.

TYPES : BEGIN OF ts_hrp1000,
          otype    TYPE hrp1000-otype,
          objid    TYPE hrp1000-objid,
          mc_seark TYPE hrp1000-mc_seark,   " Deparment
        END OF ts_hrp1000.

TYPES : BEGIN OF ts_apprv,
          otype     TYPE hrp1001-otype,
          objid     TYPE hrp1001-objid,
          subty     TYPE hrp1001-subty,
*          sobid TYPE hrp1001-sobid,
          sobid(10) TYPE c, "hrp1001-objid,
        END OF ts_apprv.


TYPES :BEGIN OF ts_t526,
         werks TYPE t526-werks,
         sachx TYPE t526-sachx,
         usrid TYPE t526-usrid,
       END OF ts_t526.

DATA: it_t526 TYPE TABLE OF ts_t526,
      wa_t526 TYPE ts_t526.

TYPES : BEGIN OF ts_apprv2,
          otype     TYPE hrp1001-otype,
          objid     TYPE hrp1001-objid,
          subty     TYPE hrp1001-subty,
*          sobid TYPE hrp1001-sobid,
          sobid(10) TYPE c,
        END OF ts_apprv2.

*TYPES : BEGIN OF ts_apprv3,
*          otype TYPE hrp1001-otype,
*          objid TYPE hrp1001-objid,
*          subty TYPE hrp1001-subty,
**          sobid TYPE hrp1001-sobid,
*            sobid TYPE hrp1001-objid,
*        END OF ts_apprv3.

DATA : it_apprv1 TYPE TABLE OF ts_apprv,
       wa_apprv1 TYPE ts_apprv.
DATA : it_apprv2 TYPE TABLE OF ts_apprv2,
       wa_apprv2 TYPE ts_apprv2.
DATA : it_apprv3 TYPE TABLE OF ts_apprv,
       wa_apprv3 TYPE ts_apprv.


TYPES: BEGIN OF ts_ptrv_shdr,
         pernr      TYPE ptrv_head-pernr,
         reinr      TYPE ptrv_head-reinr,
         sum_reimbu TYPE ptrv_shdr-sum_reimbu,
       END OF ts_ptrv_shdr.


DATA: it_swihead TYPE TABLE OF ts_swwwihead,
      wa_swihead TYPE ts_swwwihead.

DATA: it_swihead2 TYPE TABLE OF ts_swwwihead,
      wa_swihead2 TYPE ts_swwwihead.


DATA: it_ptrv TYPE TABLE OF ts_ptrv_head,
      wa_ptrv TYPE ts_ptrv_head.

DATA: it_pa0001 TYPE TABLE OF ts_ptrv_head,
      wa_pa0001 TYPE ts_ptrv_head.

DATA: it_hrp1000 TYPE TABLE OF ts_hrp1000,
      wa_hrp1000 TYPE ts_hrp1000.

DATA: it_ptrv_shdr TYPE TABLE OF ts_ptrv_shdr,
      wa_ptrv_shdr TYPE ts_ptrv_shdr.

TYPES : BEGIN OF ts_final,
          wi_id      TYPE swwwihead-wi_id,
          wi_text    TYPE swwwihead-wi_text,
          wi_rhtext  TYPE swwwihead-wi_rhtext,
          wi_stat    TYPE swwwihead-wi_stat,
          wi_cd      TYPE swwwihead-wi_cd,
          wi_ct      TYPE swwwihead-wi_ct,
          wi_aed     TYPE swwwihead-wi_aed,
          wi_aagent  TYPE swwwihead-wi_aagent,
          wi_cruser  TYPE swwwihead-wi_cruser,
          top_task   TYPE swwwihead-top_task,
          top_wi_id  TYPE swwwihead-top_wi_id,
          status     TYPE char20,
          empno(10)  TYPE c,
          tripno(15) TYPE c,
          result(4)  TYPE c,
          ename      TYPE pa0001-ename,
          pernr      TYPE ptrv_head-pernr,
          reinr      TYPE ptrv_head-reinr,
          dates      TYPE ptrv_head-dates,
          datv1      TYPE ptrv_head-datv1,
          datb1      TYPE ptrv_head-datb1,
          sum_reimbu TYPE ptrv_shdr-sum_reimbu,
          sname      TYPE pa0001-sname,
          kostl      TYPE pa0001-kostl,
          orgeh      TYPE pa0001-orgeh,
          mc_seark   TYPE hrp1000-mc_seark,   " Deparment
          ktext      TYPE cskt-ktext,
          wi_agname  TYPE pa0001-ename,
          wi_crname  TYPE pa0001-ename,
          apprv1     TYPE hrp1001-sobid,
          apprv2     TYPE hrp1001-sobid,
          apprv3     TYPE hrp1001-sobid,
          apprv4     TYPE t526-usrid,
          apprv_dt1  TYPE swwwihead-wi_aed,
          apprv_dt2  TYPE swwwihead-wi_aed,
          apprv_dt3 TYPE swwwihead-wi_aed,
          apprv_dt4  TYPE swwwihead-wi_aed,
        END OF ts_final.

DATA: it_final TYPE TABLE OF ts_final,
      wa_final TYPE ts_final.

DATA : it_fcat      TYPE slis_t_fieldcat_alv,
       wa_fcat      TYPE slis_fieldcat_alv,
       it_header    TYPE slis_t_listheader,
       wa_header    TYPE slis_listheader,
       count        TYPE i,
       wa_layout    TYPE slis_layout_alv,
       wa_cellcolor TYPE lvc_s_scol.


DATA: emp_no(10)  TYPE c,
      trip_no(15) TYPE c,
      result(4)   TYPE c,
      status      TYPE char20.


INITIALIZATION.

  "-----------------select option-------------------------

  SELECTION-SCREEN: BEGIN OF BLOCK b1.
  SELECT-OPTIONS : s_cd FOR swwwihead-wi_cd,
                   s_cruser FOR swwwihead-wi_cruser.
*                    s_top FOR swwwihead-top_wi_id.
*                    s_pernr FOR ptrv_head-pernr,
*                   s_datv1 FOR ptrv_head-datv1,
*                   s_reinr FOR ptrv_head-reinr.
  SELECTION-SCREEN: END OF BLOCK b1.


START-OF-SELECTION.

  PERFORM get_data.
  PERFORM fcat.
  PERFORM display.


*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .


  SELECT
    wi_id
    wi_type
    wi_cd
    wi_cruser
    top_wi_id
    FROM swwwihead
    INTO TABLE it_top
    WHERE wi_cd IN s_cd
    AND wi_cruser IN s_cruser
    AND wi_type = 'F'.


  MOVE-CORRESPONDING it_top TO it_top1[] KEEPING TARGET LINES.

  LOOP AT it_top1 INTO wa_top1.

    wa_top1-wi_cruser1 = wa_top1-wi_cruser.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_top1-wi_cruser1
      IMPORTING
        output = wa_top1-wi_cruser1.
    MODIFY it_top1 FROM wa_top1 INDEX sy-tabix.


  ENDLOOP.

*    select
*      a~wi_id
*      a~wi_type
*      a~wi_cd
*      a~wi_cruser
*      a~top_wi_id
*      b~sname
*      b~kostl
*      b~orgeh
*      FROM swwwihead as a
*      INNER JOIN pa0001 as b
*      ON a~wi_cruser = b~pernr
*      INTO TABLE it_top
*          WHERE a~wi_cd IN s_cd
*    AND a~wi_cruser IN s_cruser
*    AND a~wi_type = 'F'.



  IF  it_top[] IS NOT INITIAL.

    SELECT               "CI_NOFIELD
      wi_id
      wi_type
      wi_text
      wi_rhtext
      wi_stat
      wi_cd
      wi_ct
      wi_aed
      wi_aagent
*      wi_cruser
      top_wi_id
      top_task
      FROM swwwihead
      INTO TABLE it_swihead
      FOR ALL ENTRIES IN it_top
*    WHERE wi_cd = s_cd
      WHERE top_wi_id = it_top-top_wi_id
*      AND wi_type = 'W'
      AND top_task = 'WS90000009'.

    SELECT               "CI_NOFIELD
      wi_id
      wi_type
      wi_text
      wi_rhtext
      wi_stat
      wi_cd
      wi_ct
      wi_aed
      wi_aagent
*      wi_cruser
      top_wi_id
      top_task
      FROM swwwihead
      INTO TABLE it_swihead2
      FOR ALL ENTRIES IN it_swihead
      WHERE top_wi_id = it_swihead-top_wi_id
      AND wi_type = 'W'
      AND top_task = 'WS90000009'.

    IF it_swihead[] IS NOT INITIAL.

      SELECT
         pernr
         sname
         kostl
         orgeh
        werks
         plans
        sbmod
        FROM pa0001
        INTO TABLE it_pa0001
        FOR ALL ENTRIES IN it_top1
        WHERE pernr = it_top1-wi_cruser1
        and begda LE  s_cd-low
        and endda GE  s_cd-high.
    ENDIF.



    IF it_pa0001[] IS NOT INITIAL.

      SELECT
       otype
       objid
       mc_seark
       FROM hrp1000
       INTO TABLE it_hrp1000
       FOR ALL ENTRIES IN it_pa0001
       WHERE objid = it_pa0001-orgeh
       AND otype = 'O'.

      SELECT
        otype
        objid
        subty
        sobid
        FROM
        hrp1001
        INTO TABLE it_apprv1
        FOR ALL ENTRIES IN it_pa0001
        WHERE objid = it_pa0001-plans
        AND otype = 'S'
        AND subty = 'A002'.

*      SELECT
*  otype
*  objid
*  subty
*  sobid
*  FROM
*  hrp1001
*  INTO TABLE it_apprv2
*  FOR ALL ENTRIES IN it_apprv1
*  WHERE objid = it_apprv1-sobid
*  AND otype = 'S'
*  AND subty = 'A002'.

*      SELECT
*        otype
*        objid
*        subty
*        sobid
*        FROM
*        hrp1001
*        INTO TABLE it_apprv3
*        FOR ALL ENTRIES IN it_apprv2
*        WHERE objid = it_apprv2-sobid
*        AND otype = 'S'
*        AND subty = 'A002'.
*
      SELECT
        werks
        sachx
        usrid
        FROM t526
        INTO TABLE it_t526
        FOR ALL ENTRIES IN it_pa0001
        WHERE werks = it_pa0001-sbmod
        AND sachx = 'AAA'.

  SELECT
    pernr
    reinr
    dates
    datv1
    uhrv1
    datb1
    FROM ptrv_head
    INTO TABLE it_date
    FOR ALL ENTRIES IN it_pa0001
    WHERE pernr = it_pa0001-pernr.

      SELECT
       pernr
       reinr
       sum_reimbu
       FROM ptrv_shdr
       INTO TABLE it_ptrv_shdr
       FOR ALL ENTRIES IN it_pa0001
       WHERE pernr = it_pa0001-pernr.
*
    ENDIF.


  ENDIF.


  DATA: lv_wid_read     TYPE sww_wiid.

  DATA: l_it_wi_container TYPE STANDARD TABLE OF swcont,
        l_wa_wi_container TYPE swcont,
        l_wa_wi_header    TYPE swwwihead.

*Select * From SWWWIHEAD INTO TABLE lt_swwwihead
*Where date = s_date.                                      " As per selection criteria

  LOOP AT it_swihead INTO wa_swihead WHERE wi_type = 'W'.

    CLEAR: l_it_wi_container[], lv_wid_read,l_wa_wi_header.

*    IF wa_swihead-wi_aagent CA '0123456789'.    "commented on 30.09.2020

      lv_wid_read = wa_swihead-wi_id.

      CALL FUNCTION 'SWW_WI_CONTAINER_READ'
        EXPORTING
          wi_id                    = lv_wid_read
        TABLES
          wi_container             = l_it_wi_container
        CHANGING
          wi_header                = l_wa_wi_header
        EXCEPTIONS
          container_does_not_exist = 1
          read_failed              = 2
          OTHERS                   = 3.


      IF sy-subrc = 0.

        CLEAR: l_wa_wi_container.
        READ TABLE l_it_wi_container INTO l_wa_wi_container
        WITH KEY element = 'EMPLOYEENUMBER'.
        IF sy-subrc = 0.
          CONDENSE l_wa_wi_container-value.
          emp_no = l_wa_wi_container-value.
        ENDIF.

        CLEAR: l_wa_wi_container.
        READ TABLE l_it_wi_container INTO l_wa_wi_container
        WITH KEY element = 'TRIPNUMBER'.
        IF sy-subrc = 0.
          CONDENSE l_wa_wi_container-value.
          trip_no = l_wa_wi_container-value.
        ENDIF.


        CLEAR: l_wa_wi_container.
        READ TABLE l_it_wi_container INTO l_wa_wi_container
        WITH KEY element = '_RESULT'.
        IF sy-subrc = 0.
          CONDENSE l_wa_wi_container-value.
          result = l_wa_wi_container-value.
        ENDIF.

        IF trip_no = ' '.

          CLEAR: l_it_wi_container[],l_wa_wi_container, l_wa_wi_header.

          lv_wid_read = wa_swihead-wi_id + 1.

          CALL FUNCTION 'SWW_WI_CONTAINER_READ'
            EXPORTING
              wi_id                    = lv_wid_read
            TABLES
              wi_container             = l_it_wi_container
            CHANGING
              wi_header                = l_wa_wi_header
            EXCEPTIONS
              container_does_not_exist = 1
              read_failed              = 2
              OTHERS                   = 3.


          IF sy-subrc = 0.
            CLEAR: l_wa_wi_container.


            DELETE l_it_wi_container WHERE tab_index NE '000007' .

            READ TABLE l_it_wi_container INTO l_wa_wi_container  INDEX 1.
*        WITH KEY element = 'RESULT'
*                 tab_index = '7'.
            IF sy-subrc = 0.
              CONDENSE l_wa_wi_container-value.
              trip_no = l_wa_wi_container-value.
            ENDIF.
          ENDIF.
        ENDIF.


        wa_final-empno = emp_no.
        wa_final-tripno = trip_no.
        wa_final-result = result.
*        wa_final-status = status.

      ENDIF.

      wa_final-wi_id     = wa_swihead-wi_id.
      wa_final-wi_text   = wa_swihead-wi_text.
      wa_final-wi_rhtext = wa_swihead-wi_rhtext.
      wa_final-wi_stat   = wa_swihead-wi_stat.
      wa_final-wi_cd     = wa_swihead-wi_cd.
      wa_final-wi_ct     = wa_swihead-wi_ct.
      wa_final-wi_aed    = wa_swihead-wi_aed.
      wa_final-wi_aagent = wa_swihead-wi_aagent.
*      wa_final-wi_cruser = wa_swihead-wi_cruser.
      wa_final-top_task  = wa_swihead-top_task.
      wa_final-top_wi_id = wa_swihead-top_wi_id.

      READ TABLE it_top INTO wa_top WITH KEY wi_id = wa_final-top_wi_id.
      IF sy-subrc = 0.

        wa_final-wi_cruser = wa_top-wi_cruser.

      ENDIF.

      READ TABLE it_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_final-wi_cruser.
      IF  sy-subrc = 0.

        wa_final-sname = wa_pa0001-sname.
        wa_final-kostl = wa_pa0001-kostl.  "cc code
        wa_final-orgeh = wa_pa0001-orgeh.
      ENDIF.

      READ TABLE it_hrp1000 INTO wa_hrp1000 WITH KEY objid = wa_final-orgeh
                                                      otype = 'O'.
      IF sy-subrc = 0.
        wa_final-mc_seark = wa_hrp1000-mc_seark.
      ENDIF.

      READ TABLE it_ptrv_shdr INTO wa_ptrv_shdr WITH KEY pernr = wa_final-empno
                                                               reinr = wa_final-tripno.
      IF sy-subrc = 0.
        wa_final-sum_reimbu = wa_ptrv_shdr-sum_reimbu.
      ENDIF.

      READ TABLE it_date INTO wa_date with KEY pernr = wa_final-empno
                                               reinr = wa_final-tripno.
      IF sy-subrc = 0.
          wa_final-dates = wa_date-dates.
          wa_final-datv1 = wa_date-datv1.
          wa_final-datb1 = wa_date-datb1.

      ENDIF.

      READ TABLE it_swihead INTO wa_swihead WITH KEY "wi_id = wa_final-wi_id + 1
                                                     top_wi_id = wa_final-top_wi_id
                                                      wi_type = 'B'.
      IF sy-subrc = 0 .
        wa_final-status = wa_swihead-wi_rhtext.
      ENDIF.

      SELECT SINGLE ktext FROM cskt
        INTO wa_final-ktext
         WHERE kostl = wa_final-kostl.

      SELECT SINGLE ename
        FROM pa0001 INTO wa_final-wi_agname
        WHERE pernr = wa_final-wi_aagent.

      SELECT SINGLE ename
        FROM pa0001 INTO wa_final-wi_crname
        WHERE pernr = wa_final-wi_cruser.


      READ TABLE it_apprv1 INTO wa_apprv1 WITH KEY objid = wa_pa0001-plans
                                                 otype  = 'S'
                                                 subty  = 'A002'.
      IF sy-subrc = 0.

        wa_final-apprv1 = wa_apprv1-sobid.

      ENDIF.

      SELECT SINGLE sobid FROM hrp1001 INTO wa_final-apprv2 WHERE objid = wa_final-apprv1
        AND otype = 'S' AND subty = 'A002'.


      SELECT SINGLE sobid FROM hrp1001 INTO wa_final-apprv3 WHERE objid = wa_final-apprv2
     AND otype = 'S' AND subty = 'A002'.


      READ TABLE it_t526 INTO wa_t526 WITH KEY werks = wa_pa0001-sbmod
                                               sachx = 'AAA'.
      IF  sy-subrc = 0.

        wa_final-apprv4 = wa_t526-usrid.

      ENDIF.

      IF WA_FINAL-APPRV1 NE ' ' .

       READ TABLE it_swihead INTO wa_swihead with KEY wi_type = 'W'.
      IF sy-subrc = 0 AND wa_swihead-wi_cd <> '99991231'.
        wa_final-apprv_dt1 = wa_swihead-wi_aed.
      ELSEIF wa_swihead-wi_cd = '99991231'.
        lv_wid_read = wa_swihead-wi_id + 1.
      READ TABLE it_swihead INTO wa_swihead with KEY  wi_id = lv_wid_read
                                                      wi_type = 'B'.
       wa_final-apprv_dt1 = wa_swihead-wi_aed.
      ENDIF.

      ENDIF.

      IF WA_FINAL-APPRV2 NE ' ' .

            READ TABLE it_swihead INTO wa_swihead with KEY wi_type = 'W'.
      IF sy-subrc = 0 AND wa_swihead-wi_cd <> '99991231'.
        wa_final-apprv_dt1 = wa_swihead-wi_aed.
      ELSEIF wa_swihead-wi_cd = '99991231'.
        lv_wid_read = wa_swihead-wi_id + 1.
      READ TABLE it_swihead INTO wa_swihead with KEY  wi_id = lv_wid_read
                                                      wi_type = 'B'.
       wa_final-apprv_dt2 = wa_swihead-wi_aed.
      ENDIF.

*             READ TABLE it_swihead2 INTO wa_swihead2 INDEX 2.
*      IF sy-subrc = 0 .
*        wa_final-apprv_dt2 = wa_swihead-wi_aed.
*      ENDIF.
      ENDIF.

      IF WA_FINAL-APPRV3 NE ' ' .
            READ TABLE it_swihead INTO wa_swihead with KEY wi_type = 'W'.
      IF sy-subrc = 0 AND wa_swihead-wi_cd <> '99991231'.
        wa_final-apprv_dt1 = wa_swihead-wi_aed.
      ELSEIF wa_swihead-wi_cd = '99991231'.
        lv_wid_read = wa_swihead-wi_id + 1.
      READ TABLE it_swihead INTO wa_swihead with KEY  wi_id = lv_wid_read
                                                      wi_type = 'B'.
       wa_final-apprv_dt3 = wa_swihead-wi_aed.
      ENDIF.
*
*
*             READ TABLE it_swihead2 INTO wa_swihead2 INDEX 3.
*      IF sy-subrc = 0 .
*        wa_final-apprv_dt3 = wa_swihead-wi_aed.
*      ENDIF.
      ENDIF.

        IF WA_FINAL-APPRV4 NE ' ' .

            READ TABLE it_swihead INTO wa_swihead with KEY wi_type = 'W'.
      IF sy-subrc = 0 AND wa_swihead-wi_cd <> '99991231'.
        wa_final-apprv_dt1 = wa_swihead-wi_aed.
      ELSEIF wa_swihead-wi_cd = '99991231'.
        lv_wid_read = wa_swihead-wi_id + 1.
      READ TABLE it_swihead INTO wa_swihead with KEY  wi_id = lv_wid_read
                                                      wi_type = 'B'.
       wa_final-apprv_dt4 = wa_swihead-wi_aed.
      ENDIF.

*             READ TABLE it_swihead2 INTO wa_swihead2 INDEX 4.
*      IF sy-subrc = 0 .
*        wa_final-apprv_dt4 = wa_swihead-wi_aed.
*      ENDIF.
      ENDIF.


      APPEND wa_final TO it_final.

      SORT it_final BY wi_id top_wi_id.

      CLEAR : wa_final,wa_swihead,wa_top, emp_no,trip_no,result,status, wa_ptrv_shdr, wa_hrp1000, wa_t526, wa_apprv1.

*    ELSE.
*      DELETE it_swihead INDEX 1 .
*
*    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fcat .


  wa_fcat-col_pos = '1' . "column position
  wa_fcat-fieldname = 'WI_CRUSER' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Creator' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '2' . "column position
  wa_fcat-fieldname = 'WI_CRNAME' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Creator Name' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '3' . "column position
  wa_fcat-fieldname = 'KOSTL' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'CC Code' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '4' . "column position
  wa_fcat-fieldname = 'KTEXT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'CC Name' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '5' . "column position
  wa_fcat-fieldname = 'MC_SEARK' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Deparment' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '6' . "column position
  wa_fcat-fieldname = 'TOP_TASK' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Workflow' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '7' . "column position
  wa_fcat-fieldname = 'TOP_WI_ID' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Parent ID' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '8' . "column position
  wa_fcat-fieldname = 'WI_ID' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Work Item ID' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .


  wa_fcat-col_pos = '9' . "column position
  wa_fcat-fieldname = 'TRIPNO' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Trip no' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .


*  wa_fcat-col_pos = '10' . "column position
*  wa_fcat-fieldname = 'WI_AAGENT' . "column name
*  wa_fcat-tabname = 'IT_FINAL' . "table
*  wa_fcat-seltext_m = 'Approver' . "Column label
*  APPEND wa_fcat TO it_fcat . "append to fcat
*  CLEAR wa_fcat .


*  wa_fcat-col_pos = '11' . "column position
*  wa_fcat-fieldname = 'WI_AGNAME' . "column name
*  wa_fcat-tabname = 'IT_FINAL' . "table
*  wa_fcat-seltext_m = 'Approver Name' . "Column label
*  APPEND wa_fcat TO it_fcat . "append to fcat
*  CLEAR wa_fcat .

  wa_fcat-col_pos = '10' . "column position
  wa_fcat-fieldname = 'APPRV1' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approval1 Code' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

    wa_fcat-col_pos = '11' . "column position
  wa_fcat-fieldname = 'APPRV_DT1' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approval1 Date' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '12' . "column position
  wa_fcat-fieldname = 'APPRV2' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approval2 Code' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

    wa_fcat-col_pos = '13' . "column position
  wa_fcat-fieldname = 'APPRV_DT2' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approval2 Date' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '14' . "column position
  wa_fcat-fieldname = 'APPRV3' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approval3 Code' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

    wa_fcat-col_pos = '15' . "column position
  wa_fcat-fieldname = 'APPRV_DT3' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approval3 Date' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '16' . "column position
  wa_fcat-fieldname = 'APPRV4' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approval4 Code' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '17' . "column position
  wa_fcat-fieldname = 'APPRV_DT4' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approval4 Date' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '18' . "column position
  wa_fcat-fieldname = 'STATUS' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approval Status' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

*    wa_fcat-col_pos = '20' . "column position
*  wa_fcat-fieldname = 'DATES' . "column name
*  wa_fcat-tabname = 'IT_FINAL' . "table
*  wa_fcat-seltext_m = 'Approval Date' . "Column label
*  APPEND wa_fcat TO it_fcat . "append to fcat
*  CLEAR wa_fcat .

    wa_fcat-col_pos = '19' . "column position
  wa_fcat-fieldname = 'DATV1' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Date From' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

      wa_fcat-col_pos = '20' . "column position
  wa_fcat-fieldname = 'DATB1' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Date To' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '21' . "column position
  wa_fcat-fieldname = 'WI_STAT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Workflow Status' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '22' . "column position
  wa_fcat-fieldname = 'WI_TEXT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Work Item text' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .


  wa_fcat-col_pos = '23' . "column position
  wa_fcat-fieldname = 'WI_RHTEXT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Task Test' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .


  wa_fcat-col_pos = '24' . "column position
  wa_fcat-fieldname = 'WI_CD' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Creation Date' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '25' . "column position
  wa_fcat-fieldname = 'WI_CT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Creation Time' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '26' . "column position
  wa_fcat-fieldname = 'WI_AED' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approved On' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .


  wa_fcat-col_pos = '27' . "column position
  wa_fcat-fieldname = 'SUM_REIMBU' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Amount' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      it_fieldcat        = it_fcat "PASS FIELD CATALOG TO ALV
    TABLES
      t_outtab           = it_final. "output table


ENDFORM.
