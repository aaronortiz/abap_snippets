CLASS zcl_range_helper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPE-POOLS abap.

    CLASS-METHODS range_add_from_table
      IMPORTING source_table TYPE ANY TABLE
                source_field TYPE fieldnam
      CHANGING  ranges_table TYPE ANY TABLE.

ENDCLASS.

CLASS zcl_range_helper IMPLEMENTATION.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_SD_VARIABLES=>RANGE_ADD_FROM_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] SOURCE_TABLE                   TYPE        ANY TABLE
* | [--->] SOURCE_FIELD                   TYPE        FIELDNAM
* | [<-->] RANGES_TABLE                   TYPE        ANY TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD range_add_from_table.

    FIELD-SYMBOLS:
      <ls_line>   TYPE any,
      <lv_value>  TYPE any,
      <ls_range>  TYPE any,
      <lv_sign>   TYPE any,
      <lv_option> TYPE any,
      <lv_low>    TYPE any.

    DATA:
      ls_line       TYPE REF TO data,
      lv_value      TYPE REF TO data,
      ls_range_line TYPE REF TO data.

    CREATE DATA ls_range_line LIKE LINE OF ranges_table.
    ASSIGN ls_range_line->* TO <ls_range>.
    IF <ls_range> IS ASSIGNED.
      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_range> TO <lv_sign>.
      IF <lv_sign> IS ASSIGNED.
        ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_range> TO <lv_option>.
        IF <lv_option> IS ASSIGNED.
          ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_range> TO <lv_low>.
          IF <lv_low> IS ASSIGNED.

            CLEAR <ls_range>.
            <lv_sign> = 'I'.
            <lv_option> = 'EQ'.
            LOOP AT source_table ASSIGNING <ls_line>.
              IF <ls_line> IS ASSIGNED.
                ASSIGN COMPONENT source_field OF STRUCTURE <ls_line> TO <lv_value>.
                IF <lv_value> IS ASSIGNED.
                  <lv_low> = <lv_value>.
                  COLLECT <ls_range> INTO ranges_table.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.