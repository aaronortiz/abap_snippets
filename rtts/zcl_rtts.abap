CLASS zcl_rtts DEFINITION
  PUBLIC
  CREATE PUBLIC.
  
  PUBLIC SECTION.
  
    TYPE-POOLS abap.
    
    CLASS-METHODS dd_field_list_get_string
      IMPORTING destination_table TYPE ANY TABLE
      RETURNING VALUE(field_list) TYPE string.
      
ENDCLASS.

CLASS zcl_rtts IMPLEMENTATION

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_SD_VARIABLES=>DD_FIELD_LIST_GET_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] DESTINATION_TABLE              TYPE        ANY TABLE
* | [<-()] FIELD_LIST                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dd_field_list_get_string.

    DATA:
      lo_tdescr    TYPE REF TO cl_abap_tabledescr,
      lo_sdescr    TYPE REF TO cl_abap_structdescr,
      ls_component TYPE abap_compdescr.

    CLEAR: field_list, ls_component.

    TRY.
        lo_tdescr ?= cl_abap_tabledescr=>describe_by_data( destination_table ).
        lo_sdescr ?= lo_tdescr->get_table_line_type( ).
        LOOP AT lo_sdescr->components INTO ls_component.
          field_list = |{ field_list } { ls_component-name }|.
        ENDLOOP.

        CONDENSE field_list.
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
