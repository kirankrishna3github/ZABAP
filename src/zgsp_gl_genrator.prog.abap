*&---------------------------------------------------------------------*
*& Report ZGSP_GL_GENRATOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgsp_gl_genrator.

INCLUDE  zgsp_gl_genrator_top.
INCLUDE  zgsp_gl_genrator_ss.
INCLUDE  zgsp_gl_genrator_mm.
*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM data_retrieval.
  IF p_rad1 IS NOT INITIAL.
    PERFORM build_fieldcatalog.
    PERFORM build_layout.
    PERFORM display_alv_report.
  ENDIF.
  IF p_rad2 IS NOT INITIAL AND it_final IS NOT INITIAL.
    MESSAGE 'Temp1' TYPE 'S'.
    WRITE: 'temp1'.

    PERFORM generate_file.
  ENDIF.
