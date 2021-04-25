*--------------------------------------------------------------------*
" selection-screen
*--------------------------------------------------------------------*
selection-screen begin of screen 1001 title text-001.
selection-screen begin of block opt with frame title text-opt.
parameters:
  r_single radiobutton group opt default 'X' user-command opt modif id opt,
  r_mass   radiobutton group opt modif id opt.
selection-screen end of block opt.

selection-screen begin of block act with frame title text-act.
parameters:
  r_create radiobutton group act default 'X' user-command act modif id act,
  r_update radiobutton group act modif id act,
  r_delete radiobutton group act modif id act.
selection-screen end of block act.

selection-screen begin of block fil with frame title text-fil.
parameters p_file type string modif id fil.
selection-screen end of block fil.

selection-screen begin of block upd with frame title text-upd.
parameters p_update like lcl_app=>mv_address modif id upd.
selection-screen end of block upd.

selection-screen begin of block del with frame title text-del.
select-options s_delete for lcl_app=>mv_address no intervals modif id del.
selection-screen end of block del.

* Function key *
selection-screen: function key 1.
selection-screen end of screen 1001.
*--------------------------------------------------------------------*
