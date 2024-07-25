class ZCL_NEW_GTTAB_RH definition
  public
  inheriting from CL_REST_HTTP_HANDLER
  final
  create public .

public section.

  methods IF_REST_APPLICATION~GET_ROOT_HANDLER
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NEW_GTTAB_RH IMPLEMENTATION.


  METHOD if_rest_application~get_root_handler.

"CL_REST_HTTP_HANDLER // SuperClass

    DATA(lo_router) = NEW cl_rest_router( ).

    lo_router->attach(
      EXPORTING
        iv_template      = '/GetTables'
        iv_handler_class = 'ZCL_NEW_GTTAB_RP'
    ).

    ro_root_handler = lo_router.

  ENDMETHOD.
ENDCLASS.
