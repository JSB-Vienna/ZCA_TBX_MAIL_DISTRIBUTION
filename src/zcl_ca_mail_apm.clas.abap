"! <p class="shorttext synchronized" lang="en">DO NOT USE! Tech. purpose! APACK manifest for package</p>
CLASS zcl_ca_mail_apm DEFINITION PUBLIC
                                 FINAL
                                 CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_apack_manifest.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor.
protected section.
private section.
ENDCLASS.



CLASS ZCL_CA_MAIL_APM IMPLEMENTATION.


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    zif_apack_manifest~descriptor =
         VALUE #(
            group_id        = 'itinere.at'
            artifact_id     = 'zca_tbx_mail_distribution'
            version         = '0.1.0'
            repository_type = 'abapGit'
            git_url         = 'https://github.com/JSB-Vienna/zca_tbx_mail_distribution.git'
            dependencies    =
                  VALUE #(
                        group_id       = 'itinere.at'
                      ( artifact_id    = 'zca_tbx_archiving'
                        git_url        = 'https://github.com/JSB-Vienna/zca_tbx_archiving.git'
                        version        = '0.1.0'
                        target_package = 'zca_tbx_archiving' )
                      ( artifact_id    = 'zca_tbx_base'
                        git_url        = 'https://github.com/JSB-Vienna/zca_tbx_base.git'
                        version        = '0.1.0'
                        target_package = 'zca_tbx_base' )
                      ( artifact_id    = 'zca_tbx_application_logging'
                        version        = '0.1.0'
                        git_url        = 'https://github.com/JSB-Vienna/zca_tbx_application_logging.git'
                        target_package = 'zca_tbx_application_logging' ) ) ) ##no_text.
  ENDMETHOD.                    "constructor
ENDCLASS.
