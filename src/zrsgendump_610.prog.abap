*&---------------------------------------------------------------------*
*& Report ZRSGENDUMP_610
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zrsgendump_610 message-id sn.

tables: snap, snapt, snap_beg, trdir.

************************************************************************
*                     Selection Screen                                *
************************************************************************

select-options:
s_datum for snap-datum default sy-datum,
s_uzeit for snap-uzeit,
s_ahost for snap-ahost,
s_uname for snap-uname default sy-uname,
s_mandt for snap-mandt,
s_xhold for snap-xhold,
s_errid for snapt-errid.



************************************************************************
*                     Declarations                                     *
************************************************************************

*** declaration for dump selection
data: begin of ft_struk ,
        id(2), len type i, value(256),
      end of ft_struk.



data ft like ft_struk occurs 1000.
data wa_ft like   ft_struk.


data: begin of fcc,
        id(2), ll(3), errid like snapt-errid,
      end of fcc.

field-symbols: <f>.


*** my declarations
data: sel_dump_tab like snap occurs 0.
data: wa_sel_dump like snap.
data: begin of prog ,                  "# NEEDED
        name like trdir-name,
      end of prog.
data: prog_tab like prog occurs 0.
data: i type i, j type i.

*************** End of Declarations ************************************


at selection-screen.



************************************************************************

*                     Get selected Dumps                               *

************************************************************************



  select * from snap_beg

  where seqno = '000'

  and   datum in s_datum

  and   uzeit in s_uzeit

  and   ahost in s_ahost

  and   uname in s_uname

  and   mandt in s_mandt

  and   xhold in s_xhold

  order by datum uzeit ahost uname mandt modno.



    move-corresponding snap_beg to snap.



*** get dump-id



    fcc = snap-flist.

    if ( fcc-ll co '0123456789' ) and ( fcc-ll <> '000' ) and

    ( fcc-ll <= '030' ).                                  "#EC PORTABLE

      assign fcc-errid(fcc-ll) to <f>.

      snapt-errid = <f>.

    else.

      snapt-errid = '?????'.

    endif.

    check:snapt-errid in s_errid.

    append snap to sel_dump_tab.

  endselect.


  if sy-dbcnt is initial and sel_dump_tab[] is initial.
    message e404.
  endif.








start-of-selection.

  i = sy-saprl(1). j = sy-saprl+1(1).


************************************************************************
*                     Get selected Dumps                               *
************************************************************************

  select * from snap_beg
  where seqno = '000'
  and   datum in s_datum
  and   uzeit in s_uzeit
  and   ahost in s_ahost
  and   uname in s_uname
  and   mandt in s_mandt
  and   xhold in s_xhold
  order by datum uzeit ahost uname mandt modno.

    move-corresponding snap_beg to snap.

*** get dump-id

    fcc = snap-flist.
    if ( fcc-ll co '0123456789' ) and ( fcc-ll <> '000' ) and
    ( fcc-ll <= '030' ).                                  "#EC PORTABLE
      assign fcc-errid(fcc-ll) to <f>.
      snapt-errid = <f>.
    else.
      snapt-errid = '?????'.
    endif.
    check:snapt-errid in s_errid.

*** build internal table containig the selected dumps

    append snap to sel_dump_tab.

  endselect.



************************************************************************
*                     Check Dumps                                      *
************************************************************************

  loop at sel_dump_tab into wa_sel_dump.
    perform build_ft .
    perform get_loaded_programs tables ft prog_tab.
    refresh ft.
  endloop.


  perform generate_programs tables prog_tab.

********************** Ende ********************************************



*&---------------------------------------------------------------------*
*&      Form  BUILD_FT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_ft.
****

  data: begin of rs380,

          datum            like snap-datum,
          uzeit            like snap-uzeit,
          ahost            like snap-ahost,
          uname            like snap-uname,
          mandt            like snap-mandt,
          modno            like snap-modno,

          button_today     value 'X',
          button_yesterday,
          button_total,
          button_keep,
          push_500_list,
          push_500_sele,

          dumps_today      type i,
          dumps_yesterday  type i,
          dumps_total      type i,
          dumps_hold       type i,
          snap_used_k      type i,
          snap_free_k      type i,
          snap_reorg_days  type i,

        end of rs380.


  move-corresponding wa_sel_dump to  rs380.


  select * from snap
  where datum = rs380-datum
  and   uzeit = rs380-uzeit
  and   uname = rs380-uname
  and   ahost = rs380-ahost
  and   modno = rs380-modno
  and   mandt = rs380-mandt
  order by primary key.
    perform extract_strings_from_snap.
  endselect.

endform.                               " BUILD_FT

*&---------------------------------------------------------------------*
*&      Form  EXTRACT_STRINGS_FROM_SNAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form extract_strings_from_snap.
  field-symbols: <id>, <len>, <value>.
  data: snap_error(60).

* Process field list
  do.
    if sy-index = 1.
      assign snap-flist to <id> range snap. "First string
    else.
      assign <value>+<len>(2)  to <id>."Next string
    endif.

* Check field id
    if <id> le space.                                     "#EC PORTABLE
      snap_error = text-nco.           " Short Dump not consistent
      format color col_negative.
      write: / snap_error.
      write: / wa_sel_dump-datum,  / wa_sel_dump-uzeit,
      /  wa_sel_dump-ahost, / wa_sel_dump-uname.
      skip 2.
      format color off.
      exit.
    endif.

* Short dump not complete ?
    if <id> = '%A'.
      snap_error = text-ncm.           "Short Dump not complete - not
      " too harmful
      exit.
    endif.

* End of this SNAP record ?
    if <id> = '%E' or <id> = '%M'.
      exit.
    endif.

* Check format of length field
    assign <id>+2(3)        to <len>     type 'C'.
    if <len> cn '0123456789' or <len> = '000'.
      snap_error = text-nco.           "Short Dump not consistent
      format color col_negative.
      write: / snap_error.
      write: / wa_sel_dump-datum,  / wa_sel_dump-uzeit,
      /  wa_sel_dump-ahost, / wa_sel_dump-uname.
      skip 2.
      format color off.
      exit.
    endif.
    assign <id>+5(<len>)    to <value>   type 'C'.

* Append field
    wa_ft-id     =   <id>.
    wa_ft-len    =   <len>.
    wa_ft-value  =   <value>.

    append wa_ft to ft.

  enddo.


endform.                               " EXTRACT_STRINGS_FROM_SNAP
*&---------------------------------------------------------------------*
*&      Form  get_loaded_programs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FT  text
*----------------------------------------------------------------------*
form get_loaded_programs tables   p_ft structure ft_struk
  p_prog_tab  structure prog.

  data l_wa_ft like ft_struk.
  data: dummy1(10) type c,                                  "#EC NEEDED
        dummy2(10) type c,
        prog_name  like sy-repid.

*** Bulid a table of all "loaded programs" of this dump.
*** -> globale internal table prog_tab.


  loop at p_ft into l_wa_ft where id = 'PG'.

    clear: dummy1, dummy2, prog_name.

    shift l_wa_ft-value left  deleting leading  space.

**** ugly, but I must distinguish the R/3 releases

    if i < 4.                          " R/3 Release < 40A
      split l_wa_ft-value at space into prog_name dummy1 dummy2.

    elseif ( i = 4 and j < 6 ).            " Release 40A, 40B, 45A,45B
      split l_wa_ft-value at space into dummy1 prog_name dummy2.

    elseif ( i = 4 and j = 6 ) or i >= 5.   " Release 46A, 46B, 46C, 50x
      split l_wa_ft-value at space into dummy1 dummy2 prog_name.
      translate dummy2 to upper case.
      if dummy2 <> 'PRG'. continue. endif.  " I don't generate Types

    else.
      continue.
    endif.
***
    condense prog_name no-gaps.
    move prog_name to  p_prog_tab-name.
    append p_prog_tab.
  endloop.


endform.                               " get_loaded_programs
*&---------------------------------------------------------------------*
*&      Form  generate_programs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROG_TAB  text
*----------------------------------------------------------------------*
form generate_programs tables   p_prog_tab structure prog.


*** Delete Duplicate entries
  sort p_prog_tab by name.
  delete adjacent duplicates from  p_prog_tab comparing name.

*** Some programs are always in the list. Don't generate them.
*** Especially don't generate the running generation tool


  delete p_prog_tab where name =  sy-repid   or
  name = 'RSDBRUNT'  or
  name = 'SAPDB__S'  or
  name is initial or
  name = 'SAPMSSY0'.




*** Generate all selected programs

  skip 2. uline. skip 2.
  write 'Generierte Programme: '(001).
  skip 2.

  format color col_positive.

  loop at p_prog_tab.
    select single * from trdir where name = p_prog_tab-name.
    if sy-subrc = 0.
      generate report p_prog_tab-name.
      if sy-subrc <> 0.
        format color col_negative.
      else.
        format color col_positive.
      endif.
      write: / p_prog_tab-name.
    endif.

* " Commit in periodical intervals

    sy-tabix = sy-tabix mod 10.
    if sy-tabix = 0. commit work. endif.

  endloop.

  format color off.
  skip 2. uline.

  commit work.

endform.                               " generate_programs
