*&---------------------------------------------------------------------*
*& Report Z_TKEFD_CHARG_ADAPT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_TKEFD_CHARG_ADAPT.

DELETE FROM tkefd
  WHERE fienm = 'CHARG'
    AND posit = '2'
    AND dpfie = 'WERKS'.

IF sy-subrc = 0.

  UPDATE tkefd SET posit = '2'
    WHERE fienm = 'CHARG'
      AND posit = '3'
      AND dpfie = 'CHARG'.

ENDIF.

IF sy-subrc = 0.

  COMMIT WORK.
  write: 'TKEFD for characteristic CHARG corrected!'.

ELSE.

  ROLLBACK WORK.
  write: 'Correction of TKEFD for characteristic CHARG failed!'.

ENDIF.
