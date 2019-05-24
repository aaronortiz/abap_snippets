CLASS zcl_salv_table_example DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPE-POOLS abap.

    TYPES:
      BEGIN OF output_st,
        col1 TYPE string,
        col2 TYPE string,
        col3 TYPE string,
      END OF output_st,
      
      output_tt TYPE STANDARD TABLE OF output_st WITH DEFAULT_KEY.
  
    DATA:
      output TYPE output_tt,
      alv    TYPE REF TO cl_salv_table.

    METHODS constructor
      IMPORTING t_output.

    METHODS alv_display.
    
  PRIVATE SECTION.
    
    METHODS alv_enable_layout_settings.

ENDCLASS.

CLASS zcl_salv_table_example IMPLEMENTATION.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SD_VARIABLES->ALV_DISPLAY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD alv_display.

    DATA:
      ls_line TYPE zvari_output_st,
      lt_temp TYPE STANDARD TABLE OF zvari_output_st WITH DEFAULT KEY,
      lo_exc  TYPE REF TO cx_salv_msg.

    TRY.
        lt_temp[] = me->output[].
        cl_salv_table=>factory( IMPORTING r_salv_table = alv
                                CHANGING t_table = lt_temp ).
      CATCH cx_salv_msg INTO lo_exc.
    ENDTRY.

    me->alv_enable_layout_settings( ).
    alv->display( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SD_VARIABLES->ALV_ENABLE_LAYOUT_SETTINGS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD alv_enable_layout_settings.

    DATA:
      lr_layout_settings  TYPE REF TO cl_salv_layout,
      ls_layout_key       TYPE salv_s_layout_key,
      lr_functions        TYPE REF TO cl_salv_functions_list,
      lr_display_settings TYPE REF TO cl_salv_display_settings,
      lr_columns          TYPE REF TO cl_salv_columns_table,

      lr_salv_data_error  TYPE REF TO cx_salv_data_error.

    TRY.
        lr_layout_settings = me->alv->get_layout( ).

        ls_layout_key-report = sy-repid.
        lr_layout_settings->set_key( ls_layout_key ).

        lr_layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).

        lr_columns = me->alv->get_columns( ).
        lr_columns->set_optimize( ).
        "lr_columns->set_color_column( 'COLOR_COLUMN_NAME' ). " un-comment if your table has a color column (use type LVC_T_SCOL for it)

        lr_functions = me->alv->get_functions( ).
        lr_functions->set_all( ).

        lr_display_settings = me->alv->get_display_settings( ).
        lr_display_settings->set_striped_pattern( if_salv_c_bool_sap=>true ).

      CATCH cx_salv_data_error INTO lr_salv_data_error.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
