class ZCL_TVARVC_HELPER definition
  public
  final
  create public .

*"* public components of class ZCL_TVARVC_HELPER
*"* do not include other source files here!!!
public section.

  types:
    TVARVC_TT type STANDARD TABLE OF tvarvc WITH DEFAULT KEY .

  class-methods SET
    importing
      !NAME type RVARI_VNAM
      !TYPE type RSSCR_KIND
      !NUMB type TVARV_NUMB default 0
      !SIGN type TVARV_SIGN default 'I'
      !OPTI type TVARV_OPTI default 'EQ'
      !LOW type TVARV_VAL
      !HIGH type TVARV_VAL
      !CLIE_INDEP type SYCHAR01 default '' .
  class-methods GET_LOW
    importing
      !NAME type RVARI_VNAM
      !TYPE type RSSCR_KIND default 'S'
      !NUMB type TVARV_NUMB default 0
    returning
      value(LOW) type TVARV_VAL .
  class-methods GET
    importing
      !NAME type RVARI_VNAM
      !TYPE type RSSCR_KIND default 'S'
      !NUMB type TVARV_NUMB default 0
    returning
      value(RESULTS) type TVARVC .
  class-methods CLEAR
    importing
      !NAME type RVARI_VNAM
      !TYPE type RSSCR_KIND
      !NUMB type TVARV_NUMB default 0 .

*"* dummy include to reduce generation dependencies between
*"* class ZCL_TVARVC_HELPER and it's users.
*"* touched if any type reference has been changed

*"* protected components of class ZCL_TVARVC_HELPER
*"* do not include other source files here!!!
protected section.

*"* private components of class ZCL_TVARVC_HELPER
*"* do not include other source files here!!!
private section.

ENDCLASS.


CLASS ZCL_TVARVC_HELPER% IMPLEMENTATION.
METHOD CLEAR.

***Lock table TVARVC***
  CALL FUNCTION 'ENQUEUE_E_LOCK_TVARVC'
    EXPORTING
      mode_tvarvc    = 'E'
      mandt          = sy-mandt
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc = 0.

    DELETE FROM tvarvc WHERE name = name
                         AND type = type
                         AND numb = numb.

***Unlock table TVARVC***
    CALL FUNCTION 'DEQUEUE_E_LOCK_TVARVC'
      EXPORTING
        mode_tvarvc = 'E'
        mandt       = sy-mandt.

  ENDIF.

ENDMETHOD.

METHOD get.

  SELECT SINGLE * FROM tvarvc INTO results
  WHERE name EQ name
    AND type EQ type
    AND numb EQ numb.

ENDMETHOD.

METHOD SET.

  DATA:
    ls_tvarvc TYPE tvarvc.

  ls_tvarvc-mandt = sy-mandt.
  ls_tvarvc-name = name.
  ls_tvarvc-type = type.
  ls_tvarvc-numb = numb.
  ls_tvarvc-sign = sign.
  ls_tvarvc-opti = opti.
  ls_tvarvc-low  = low.
  ls_tvarvc-high = high.
  ls_tvarvc-clie_indep = clie_indep.

***Lock table TVARVC***
  CALL FUNCTION 'ENQUEUE_E_LOCK_TVARVC'
    EXPORTING
      mode_tvarvc    = 'E'
      mandt          = sy-mandt
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc = 0.

    MODIFY tvarvc FROM ls_tvarvc.

***Unlock table TVARVC***
    CALL FUNCTION 'DEQUEUE_E_LOCK_TVARVC'
      EXPORTING
        mode_tvarvc = 'E'
        mandt       = sy-mandt.

  ENDIF.

ENDMETHOD.

METHOD GET_LOW.

  DATA: ls_results TYPE tvarvc.

  SELECT SINGLE * FROM tvarvc INTO ls_results
  WHERE name EQ name
    AND type EQ type
    AND numb EQ numb.

  low = ls_results-low.

ENDMETHOD.

ENDCLASS.
