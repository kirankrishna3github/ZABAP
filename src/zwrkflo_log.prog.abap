*&---------------------------------------------------------------------*
*& Report ZWRKFLO_LOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwrkflo_log.

TYPE-POOLS :slis.

TABLES:swwwihead, ptrv_head.


TYPES :BEGIN OF ts_top,
         wi_cd     TYPE swwwihead-wi_cd,
         top_wi_id TYPE swwwihead-top_wi_id,
       END OF ts_top.

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
         wi_cruser TYPE swwwihead-wi_cruser,
         top_wi_id TYPE swwwihead-top_wi_id,    "top level workflow
         top_task  TYPE swwwihead-top_task,
       END OF ts_swwwihead.



TYPES: BEGIN OF ts_ptrv_head,
         pernr TYPE ptrv_head-pernr,
         reinr TYPE ptrv_head-reinr,
         dates TYPE ptrv_head-dates,
         datv1 TYPE ptrv_head-datv1,
         uhrv1 TYPE ptrv_head-uhrv1,
*         sum_reimbu TYPE ptrv_head-sum_reimbu,
         sname TYPE pa0001-sname,
         kostl TYPE pa0001-kostl,
         orgeh TYPE pa0001-orgeh,
       END OF ts_ptrv_head.

TYPES : BEGIN OF ts_hrp1000,
          otype    TYPE hrp1000-otype,
          objid    TYPE hrp1000-objid,
          mc_seark TYPE hrp1000-mc_seark,   " Deparment
        END OF ts_hrp1000.

TYPES: BEGIN OF ts_ptrv_shdr,
         pernr      TYPE ptrv_shdr-pernr,
         reinr      TYPE ptrv_shdr-reinr,
         sum_reimbu TYPE ptrv_shdr-sum_reimbu,
       END OF ts_ptrv_shdr.


DATA: it_swihead TYPE TABLE OF ts_swwwihead,
      wa_swihead TYPE ts_swwwihead.



DATA: it_ptrv TYPE TABLE OF ts_ptrv_head,
      wa_ptrv TYPE ts_ptrv_head.

DATA: it_hrp1000 TYPE TABLE OF ts_hrp1000,
      wa_hrp1000 TYPE ts_hrp1000.

DATA: it_ptrv_shdr TYPE TABLE OF ts_ptrv_shdr,
      wa_ptrv_shdr TYPE ts_ptrv_shdr.

TYPES : BEGIN OF ts_final,
          wi_id      TYPE swwwihead-wi_id,
         wi_text   TYPE swwwihead-wi_text,
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
          uhrv1      TYPE ptrv_head-uhrv1,
          sum_reimbu TYPE ptrv_shdr-sum_reimbu,
          sname      TYPE pa0001-sname,
          kostl      TYPE pa0001-kostl,
          orgeh      TYPE pa0001-orgeh,
          mc_seark   TYPE hrp1000-mc_seark,   " Deparment
          ktext      TYPE cskt-ktext,
          wi_agname TYPE pa0001-ename,
          wi_crname TYPE pa0001-ename,
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
                   s_cruser FOR swwwihead-wi_cruser,
                    s_top FOR swwwihead-top_wi_id.
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

*  SELECT
*    a~pernr
*    a~reinr
*    a~dates
*    a~datv1
*    a~uhrv1
*    b~sname
*    b~kostl
*    b~orgeh
*    FROM ptrv_head AS a
*    INNER JOIN pa0001 AS b
*    ON a~pernr EQ b~pernr
*    INTO TABLE it_ptrv
*    WHERE a~pernr = s_pernr
*    AND a~datv1 = s_datv1
*    AND reinr = s_reinr.

*  IF it_ptrv[] IS NOT INITIAL.
*
*    SELECT
*      otype
*      objid
*      mc_seark
*      FROM hrp1000
*      INTO TABLE it_hrp1000
*      FOR ALL ENTRIES IN it_ptrv
*      WHERE objid = it_ptrv-orgeh
*      AND otype = 'O'.
*
*    SELECT
*      pernr
*      reinr
*      sum_reimbu
*      FROM ptrv_shdr
*      INTO TABLE it_ptrv_shdr
*      FOR ALL ENTRIES IN it_ptrv
*      WHERE pernr = it_ptrv-pernr
*      AND reinr = it_ptrv-reinr.
*
*  ENDIF.

  SELECT
    wi_cd
    top_wi_id
    FROM swwwihead
    INTO TABLE it_top
    WHERE wi_cd IN s_cd
    AND wi_cruser IN s_cruser
    AND top_wi_id IN s_top
    AND wi_type = 'F'.

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
    wi_cruser
    top_wi_id
    top_task
    FROM swwwihead
    INTO TABLE it_swihead
    FOR ALL ENTRIES IN it_top
*    WHERE wi_cd = s_cd
    WHERE top_wi_id = it_top-top_wi_id
    AND wi_type = 'W'.




  DATA: lv_wid_read     TYPE sww_wiid.

  DATA: l_it_wi_container TYPE STANDARD TABLE OF swcont,
        l_wa_wi_container TYPE swcont,
        l_wa_wi_header    TYPE swwwihead.

*Select * From SWWWIHEAD INTO TABLE lt_swwwihead
*Where date = s_date.                                      " As per selection criteria

  LOOP AT it_swihead INTO wa_swihead.

    IF wa_swihead-wi_aagent CA '0123456789'.

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
        WITH KEY element = 'RESULT'.
        IF sy-subrc = 0.
          CONDENSE l_wa_wi_container-value.
          result = l_wa_wi_container-value.
        ENDIF.


        IF  result = '0001'.
          status = ' Travel Approved'.

        ELSEIF result = '0002'.

          status = 'Travel Rejected'.
        ENDIF.

        wa_final-empno = emp_no.

*        SELECT SINGLE ename
*          FROM pa0001 INTO wa_final-ename
*           WHERE pernr = wa_final-empno.

        wa_final-tripno = trip_no.
        wa_final-result = result.
        wa_final-status = status.

      ENDIF.

      wa_final-wi_id     = wa_swihead-wi_id.
      wa_final-wi_text   = wa_swihead-wi_text.
      wa_final-wi_rhtext = wa_swihead-wi_rhtext.
      wa_final-wi_stat   = wa_swihead-wi_stat.
      wa_final-wi_cd     = wa_swihead-wi_cd.
      wa_final-wi_ct     = wa_swihead-wi_ct.
      wa_final-wi_aed    = wa_swihead-wi_aed.
      wa_final-wi_aagent = wa_swihead-wi_aagent.
      wa_final-wi_cruser = wa_swihead-wi_cruser.
      wa_final-top_task  = wa_swihead-top_task.
      wa_final-top_wi_id = wa_swihead-top_wi_id.

      SELECT SINGLE ename
        FROM pa0001 INTO wa_final-wi_agname
        WHERE pernr = wa_final-wi_aagent.

       SELECT SINGLE ename
         FROM pa0001 INTO wa_final-wi_crname
         WHERE pernr = wa_final-wi_cruser.

      APPEND wa_final TO it_final.
      CLEAR : wa_final,wa_swihead, emp_no,trip_no,result,status.

    ELSE.
      DELETE it_swihead INDEX 1 .

    ENDIF.

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
*  wa_fcat-col_pos = '1' . "column position
*  wa_fcat-fieldname = 'EMPNO' . "column name
*  wa_fcat-tabname = 'IT_FINAL' . "table
*  wa_fcat-seltext_m = 'Employee no' . "Column label
*  APPEND wa_fcat TO it_fcat . "apend to fcat
*  CLEAR wa_fcat .
*
*  wa_fcat-col_pos = '2' . "column position
*  wa_fcat-fieldname = 'ENAME' . "column name
*  wa_fcat-tabname = 'IT_FINAL' . "table
*  wa_fcat-seltext_m = 'Emp Name' . "Column label
*  APPEND wa_fcat TO it_fcat . "append to fcat
*  CLEAR wa_fcat .

  wa_fcat-col_pos = '1' . "column position
  wa_fcat-fieldname = 'TOP_TASK' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Workflow' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '2' . "column position
  wa_fcat-fieldname = 'TOP_WI_ID' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Parent ID' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '3' . "column position
  wa_fcat-fieldname = 'WI_ID' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Work Item ID' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '4' . "column position
  wa_fcat-fieldname = 'TRIPNO' . "column name
  wa_fcat-tabname = 'IT_FINAAL' . "table
  wa_fcat-seltext_m = 'Trip no' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

    wa_fcat-col_pos = '5' . "column position
  wa_fcat-fieldname = 'WI_CRUSER' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Creator' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

    wa_fcat-col_pos = '6' . "column position
  wa_fcat-fieldname = 'WI_CRNAME' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Creator Name' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .


    wa_fcat-col_pos = '7' . "column position
  wa_fcat-fieldname = 'WI_AAGENT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approver' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .


   wa_fcat-col_pos = '8' . "column position
  wa_fcat-fieldname = 'WI_AGNAME' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approver Name' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .


    wa_fcat-col_pos = '9' . "column position
  wa_fcat-fieldname = 'STATUS' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approval Status' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '10' . "column position
  wa_fcat-fieldname = 'WI_STAT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Workflow Status' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

    wa_fcat-col_pos = '11' . "column position
  wa_fcat-fieldname = 'WI_TEXT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Work Item text' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .


  wa_fcat-col_pos = '12' . "column position
  wa_fcat-fieldname = 'WI_RHTEXT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Task Test' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .


  wa_fcat-col_pos = '13' . "column position
  wa_fcat-fieldname = 'WI_CD' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Creation Date' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '14' . "column position
  wa_fcat-fieldname = 'WI_CT' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Creation Time' . "Column label
  APPEND wa_fcat TO it_fcat . "append to fcat
  CLEAR wa_fcat .

  wa_fcat-col_pos = '15' . "column position
  wa_fcat-fieldname = 'WI_AED' . "column name
  wa_fcat-tabname = 'IT_FINAL' . "table
  wa_fcat-seltext_m = 'Approved On' . "Column label
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
