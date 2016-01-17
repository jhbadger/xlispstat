#ifdef XLISP_STAT
/************************************************************************/
/************************************************************************/
/**                                                                    **/
/**                          Object Methods                            **/
/**                                                                    **/
/************************************************************************/
/************************************************************************/

/* objects.c */
{	NULL,				S, xsmessage_method		}, /* 300 */
{	NULL,				S, xsreparent_object		}, 
{	NULL,				S, xshas_slot			}, 
{	NULL,				S, xshas_method			},
{	NULL,				S, xsadd_slot			},
{	NULL,				S, xsdelete_slot		},
{	NULL,				S, xsadd_method			},
{	NULL,				S, xsdelete_method		},
{	NULL,				S, xsshow_object		},
{	NULL,				S, xsobject_isnew		},
{	NULL,				S, xsparents			},
{	NULL,				S, xsprecedence_list		},
{	NULL,				S, xsobject_slots		},
{	NULL,				S, xsobject_methods		},
{	NULL,				S, xsobject_documentation	},
{	NULL,				S, xsmakeproto			},

#ifndef NOGRAPHICS
/* hardware-objects */
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},

/* windows.c */
{	NULL,				S, xsshowwindow			},
{	NULL,				S, xshidewindow			},
{	NULL,				S, xshidewindow			},
{	NULL,				S, xswindow_title		},
{	NULL,				S, xswindow_location		},
{	NULL,				S, xswindow_size		},
{	NULL,				S, xswindow_frame_location	},
{	NULL,				S, xswindow_frame_size		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},

#ifdef MACINTOSH
/* edit.c */
{	NULL,				S, xsedit_window_isnew		},
{	NULL,				S, xsedit_window_allocate	},
{	NULL,				S, xsedit_window_cut		},
{	NULL,				S, xsedit_window_copy		},
{	NULL,				S, xsedit_window_paste		},
{	NULL,				S, xsedit_window_revert		},
{	NULL,				S, xsedit_window_save		},
{	NULL,				S, xsedit_window_save_as	},
{	NULL,				S, xsedit_window_save_copy	},
{	NULL,				S, xsedit_window_paste_stream	},
{	NULL,				S, xsedit_window_paste_string	},
{	NULL,				S, xsedit_window_flush_window	},
{	NULL,				S, xsedit_window_selection_stream},
{	NULL,				S, xsedit_window_remove		},
{	NULL,				S, xsedit_window_remove		},
{	NULL,				S, xsedit_window_remove		},
{	NULL,				S, xsedit_window_activate	},
{	NULL,				S, xsedit_window_update		},
{	NULL,				S, xsedit_window_find		},
{	NULL,				S, xslistener_isnew		},
{	NULL,				S, xslistener_allocate		},
{	NULL,				S, xshidewindow			},
{	NULL,				S, xshidewindow			},
{	NULL,				S, xshidewindow			},
#endif /* MACINTOSH */

/* menus.c */
{	NULL,				S, xsmenu_isnew			},
{	NULL,				S, xsallocate_menu		},
{	NULL,				S, xsdispose_menu		},
{	NULL,				S, xsinstall_menu		},
{	NULL,				S, xsremove_menu		},
{	NULL,				S, xsmenu_enabled		},
{	NULL,				S, xsupdate_menu		},
{	NULL,				S, xsallocated_p		},
{	NULL,				S, xsmenu_title			},
{	NULL,				S, xsmenu_items			},
{	NULL,				S, xsinstalled_p		},
{	NULL,				S, xsappend_items		},
{	NULL,				S, xsdelete_items		},
{	NULL,				S, xsmenu_select		},
{	NULL,				S, xsmenu_popup			},
#ifdef MACINTOSH
{	NULL,				S, xsapple_menu_isnew		}, 
{	NULL,				S, xsapple_menu_select		}, 
#endif /* MACINTOSH */
{	NULL,				S, xsitem_isnew			},
{	NULL,				S, xsitem_title			},
{	NULL,				S, xsitem_key			},
{	NULL,				S, xsitem_mark			},
{	NULL,				S, xsitem_style			},
{	NULL,				S, xsitem_action		},
{	NULL,				S, xsitem_enabled		},
{	NULL,				S, xsitem_installed_p		},
{	NULL,				S, xsitem_update		},
{	NULL,				S, xsitem_do_action		},

/* dialog.c */
{	NULL,				S, xsdialog_isnew		},
{	NULL,				S, xsdialog_allocate		},
{	NULL,				S, xsdialog_remove		},
{	NULL,				S, xsdialog_remove		},
{	NULL,				S, xsdialog_remove		},
{	NULL,				S, xsdialog_allocated_p		},
{	NULL,				S, xsdialog_default_button	},
{	NULL,				S, xsdialog_modal		},
{	NULL,				S, xsdialog_item_do_action	},
{	NULL,				S, xsdialog_item_action		},
{	NULL,				S, xsbutton_item_isnew		},
{	NULL,				S, xstoggle_item_isnew		},
{	NULL,				S, xstoggle_item_value		},
{	NULL,				S, xstext_item_isnew		},
{	NULL,				S, xstext_item_text		},
{	NULL,				S, xschoice_item_isnew		},
{	NULL,				S, xschoice_item_value		},
{	NULL,				S, xsscroll_item_isnew		},
{	NULL,				S, xsscroll_item_value		},
{	NULL,				S, xsscroll_item_max		},
{	NULL,				S, xsscroll_item_min		},
{	NULL,				S, xsscroll_item_action		},
{	NULL,				S, xslist_item_isnew		},
{	NULL,				S, xslist_item_action		},
{	NULL,				S, xslist_item_text		},
{	NULL,				S, xslist_item_selection	},

/* xsiviewwindow.c */
{	NULL,				S, iview_window_isnew		},
{	NULL,				S, iview_window_allocate	},

{	NULL,				S, iview_window_idle_on		},

{	NULL,				S, iview_window_menu		},

{	NULL,				S, xsiview_window_update	},
{	NULL,				S, xsiview_window_activate	},

{	NULL,				S, iview_window_remove		},
{	NULL,				S, iview_window_remove		},
{	NULL,				S, iview_window_remove		},
{	NULL,				S, iview_window_while_button_down},
{	NULL,				S, iview_window_show_window	},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},

{	NULL,				S, iview_window_canvas_width	},
{	NULL,				S, iview_window_canvas_height	},
{	NULL,				S, iview_window_line_type	},
{	NULL,				S, iview_window_draw_mode	},
{	NULL,				S, iview_window_draw_color	},
{	NULL,				S, iview_window_back_color	},
{	NULL,				S, iview_window_use_color	},
{	NULL,				S, iview_window_reverse_colors	},
{	NULL,				S, iview_window_view_rect	},
{	NULL,				S, iview_window_line_width	},
{	NULL,				S, iview_window_clip_rect	},
{	NULL,				S, iview_window_cursor		},

{	NULL,				S, iview_window_has_h_scroll	},
{	NULL,				S, iview_window_has_v_scroll	},
{	NULL,				S, iview_window_scroll		},
{	NULL,				S, iview_window_h_scroll_incs	},
{	NULL,				S, iview_window_v_scroll_incs	},

{	NULL,				S, iview_window_draw_line	},
{	NULL,				S, iview_window_draw_point	},
{	NULL,				S, iview_window_erase_rect	},
{	NULL,				S, iview_window_frame_rect	},
{	NULL,				S, iview_window_paint_rect	},
{	NULL,				S, iview_window_erase_oval	},
{	NULL,				S, iview_window_frame_oval	},
{	NULL,				S, iview_window_paint_oval	},
{	NULL,				S, iview_window_erase_arc	},
{	NULL,				S, iview_window_frame_arc	},
{	NULL,				S, iview_window_paint_arc	},
{	NULL,				S, iview_window_erase_poly	},
{	NULL,				S, iview_window_frame_poly	},
{	NULL,				S, iview_window_paint_poly	},

{	NULL,				S, iview_window_text_ascent	},
{	NULL,				S, iview_window_text_descent	},
{	NULL,				S, iview_window_text_width	},
{	NULL,				S, iview_window_draw_string	},
{	NULL,				S, iview_window_draw_string_up	},
{	NULL,				S, iview_window_draw_text	},
{	NULL,				S, iview_window_draw_text_up	},

{	NULL,				S, iview_window_draw_symbol	},
{	NULL,				S, iview_window_replace_symbol	},

{	NULL,				S, iview_window_start_buffering	},
{	NULL,				S, iview_window_buffer_to_screen},

#ifdef MACINTOSH
{	NULL,				S, iview_window_copy_to_clip	},
#endif /* MACINTOSH */
{	NULL,				S, iview_window_drag_grey_rect	},
{	NULL,				S, iview_window_dump_image	},
{	NULL,				S, gw_draw_bitmap		},

/* xsiview.c */
{	NULL,				S, iview_isnew			},
{	NULL,				S, iview_allocate		},

{	NULL,				S, iview_std_resize		},
{	NULL,				S, iview_std_redraw		},
{	NULL,				S, iview_std_redraw_background	},
{	NULL,				S, iview_std_clear_content	},
{	NULL,				S, iview_std_redraw_content	},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, iview_std_adjust_screen	},
{	NULL,				S, iview_std_adjust_points_in_rect},
{	NULL,				S, iview_std_adjust_screen_point},
{	NULL,				S, iview_std_mark_points_in_rect},

{	NULL,				S, iview_content_rect		},
{	NULL,				S, iview_content_origin		},
{	NULL,				S, iview_content_variables	},
{	NULL,				S, iview_click_range		},
{	NULL,				S, iview_mouse_mode		},
{	NULL,				S, iview_showing_labels		},
{	NULL,				S, iview_margin			},
{	NULL,				S, iview_fixed_aspect		},
{	NULL,				S, iview_dirty			},

{	NULL,				S, iview_x_axis			},
{	NULL,				S, iview_y_axis			},

{	NULL,				S, iview_brush			},
{	NULL,				S, iview_erase_brush		},
{	NULL,				S, iview_draw_brush		},
{	NULL,				S, iview_move_brush		},
{	NULL,				S, iview_resize_brush		},

{	NULL,				S, iview_do_click		},
{	NULL,				S, iview_do_motion		},
{	NULL,				S, iview_std_click		},
{	NULL,				S, iview_std_click		},
{	NULL,				S, iview_std_motion		},
{	NULL,				S, iview_unselect_all_points	},
{	NULL,				S, iview_erase_selection	},
{	NULL,				S, iview_mask_selection		},
{	NULL,				S, iview_unmask_all_points	},
{	NULL,				S, iview_points_showing		},
{	NULL,				S, iview_points_hilited		},
{	NULL,				S, iview_points_selected	},
{	NULL,				S, iview_points_selected	},
{	NULL,				S, iview_show_all_points	},
{	NULL,				S, iview_all_points_showing	},
{	NULL,				S, iview_all_points_unmasked	},
{	NULL,				S, iview_any_points_selected	},

{	NULL,				S, iview_linked			},
{	NULL,				S, iview_links			},

{	NULL,				S, iview_num_variables		},
{	NULL,				S, iview_variable_label		},
{	NULL,				S, iview_range			},
{	NULL,				S, iview_scaled_range		},
{	NULL,				S, iview_screen_range		},
{	NULL,				S, iview_transformation		},
{	NULL,				S, iview_apply_transformation	},

{	NULL,				S, iview_add_points		},
{	NULL,				S, iview_clear_points		},
{	NULL,				S, iview_num_points		},
{	NULL,				S, iview_point_coordinate	},
{	NULL,				S, iview_point_screen_coordinate},
{	NULL,				S, iview_point_transformed_coordinate	},
{	NULL,				S, iview_point_masked		},
{	NULL,				S, iview_point_color		},
{	NULL,				S, iview_point_state		},
{	NULL,				S, iview_point_screen_state	},
{	NULL,				S, iview_point_marked		},
{	NULL,				S, iview_point_label		},
{	NULL,				S, iview_point_symbol		},
{	NULL,				S, iview_point_selected		},
{	NULL,				S, iview_point_hilited		},
{	NULL,				S, iview_point_showing		},

{	NULL,				S, iview_add_lines		},
{	NULL,				S, iview_clear_lines		},
{	NULL,				S, iview_num_lines		},
{	NULL,				S, iview_line_coordinate	},
{	NULL,				S, iview_line_screen_coordinate	},
{	NULL,				S, iview_line_transformed_coordinate	},
{	NULL,				S, iview_line_masked		},
{	NULL,				S, iview_line_color		},
{	NULL,				S, iview_line_next		},
{	NULL,				S, iview_line_type		},
{	NULL,				S, iview_line_width		},

#ifdef USESTRINGS
{	NULL,				S, iview_add_strings		},
{	NULL,				S, iview_clear_strings		},
{	NULL,				S, iview_num_strings		},
{	NULL,				S, iview_string_coordinate	},
{	NULL,				S, iview_string_screen_coordinate},
{	NULL,				S, iview_string_transformed_coordinate	},
{	NULL,				S, iview_string_masked		},
{	NULL,				S, iview_string_color		},
{	NULL,				S, iview_string_modifiers	},
#endif /* USESTRINGS */

{	NULL,				S, iview_draw_data_points	},
{	NULL,				S, iview_draw_data_lines	},
#ifdef USESTRINGS
{	NULL,				S, iview_draw_data_strings	},
#endif /* USESTRINGS */

{	NULL,				S, iview_rotate_2		},

{	NULL,				S, iview_adjust_to_data		},
{	NULL,				S, iview_visible_range		},
{	NULL,				S, iview_scale_to_range		},
{	NULL,				S, iview_scale			},
{	NULL,				S, iview_shift			},

{	NULL,				S, iview_clear_masks		},
{	NULL,				S, iview_slice_variable		},
{	NULL,				S, iview_real_to_screen		},
{	NULL,				S, iview_screen_to_real		},
{	NULL,				S, iview_scaled_to_screen	},
{	NULL,				S, iview_screen_to_scaled	},
{	NULL,				S, iview_points_in_rect		},
{	NULL,				S, iview_adjust_depth_cuing	},

{	NULL,				S, iview_spin_allocate		},
{	NULL,				S, iview_spin_content_variables	},
{	NULL,				S, iview_spin_showing_axes	},
{	NULL,				S, iview_spin_depth_cuing	},
{	NULL,				S, iview_spin_resize		},
{	NULL,				S, iview_spin_redraw_content	},
{	NULL,				S, iview_spin_rotate		},
{	NULL,				S, iview_spin_angle		},
{	NULL,				S, iview_spin_rotate		},
{	NULL,				S, iview_spin_draw_axes		},

{	NULL,				S, iview_scatmat_allocate	},
{	NULL,				S, iview_scatmat_resize		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, iview_scatmat_redraw_content	},
{	NULL,				S, iview_scatmat_click		},
{	NULL,				S, iview_scatmat_motion		},
{	NULL,				S, iview_scatmat_add_points	},
{	NULL,				S, iview_scatmat_add_lines	},
#ifdef USESTRINGS
{	NULL,				S, iview_scatmat_add_strings	},
#endif /* USESTRINGS */
{	NULL,				S, iview_scatmat_adjust_screen_point},
{	NULL,				S, iview_scatmat_adjust_points_in_rect},
{	NULL,				S, iview_scatmat_mark_points_in_rect},

{	NULL,				S, iview_list_allocate		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, iview_list_redraw_content	},
{	NULL,				S, iview_list_add_points	},
{	NULL,				S, iview_list_adjust_screen_point},
{	NULL,				S, iview_list_adjust_points_in_rect},
{	NULL,				S, iview_list_mark_points_in_rect},

{	NULL,				S, iview_hist_isnew		},
{	NULL,				S, iview_hist_allocate		},
{	NULL,				S, iview_hist_add_points	},
{	NULL,				S, iview_hist_clear_points	},
{	NULL,				S, iview_hist_resize		},
{	NULL,				S, iview_hist_redraw_content	},
{	NULL,				S, iview_hist_adjust_screen	},
{	NULL,				S, iview_hist_num_bins		},
{	NULL,				S, iview_hist_bin_counts	},
{	NULL,				S, iview_hist_adjust_to_data	},
{	NULL,				S, iview_hist_adjust_screen_point},
{	NULL,				S, iview_hist_adjust_points_in_rect},
{	NULL,				S, iview_hist_mark_points_in_rect},

{	NULL,				S, iview_plot2d_add_points	},
{	NULL,				S, iview_plot2d_add_lines	},
#ifdef USESTRINGS
{	NULL,				S, iview_plot2d_add_strings	},
#endif /* USESTRINGS */
{	NULL,				S, iview_plot2d_adjust_to_data	},
#endif /* NOGRAPHICS */

/* compound.c */
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},

/************************************************************************/
/************************************************************************/
/**                                                                    **/
/**                        Reguler Functions                           **/
/**                                                                    **/
/************************************************************************/
/************************************************************************/

/* dialog.c */
{	"SYSBEEP",			S, xssysbeep			},
#ifdef MACINTOSH
{	"ABOUT-XLISP-STAT",		S, xsabout_xlisp_stat		},
{	"LAUNCH-APPLICATION",		S, xlaunchapp			},
{	"ENCODE-SIGNATURE",		S, xstrtoostype			},
{	"GET-FRONT-PROCESS",		S, xgetfrontprocess		},
{	"SET-FRONT-PROCESS",		S, xsetfrontprocess		},
{	"GET-PROCESS-LIST",		S, xgetprocesslist		},
{	"GET-APPLE-EVENT-TARGET",	S, xgettarget			},
{	"GET-APPLE-EVENT-TARGET-LIST",	S, xgettargetlist		},
{	"BROWSE-APPLE-EVENT-TARGETS",	S, xbrowsetargets		},
{	"SEND-APPLE-EVENT",		S, xsendappleevent		},
{	"START-SESSION",		S, xppcstartup			},

/* edit.c */
{	"OPEN-FILE-DIALOG",		S, xsopenfiledialog		},
{	"SET-FILE-DIALOG",		S, xssetfiledialog		},
{	"SET-VOLUME",			S, xssetvolume			},
{	"FRONT-WINDOW",			S, xsfrontwindow		},
{	"HIDE-FRONT-WINDOW",		S, xshidefrontwindow		},
{	"SYSTEM-EDIT",			S, xssystem_edit		},
#endif /* MACINTOSH */
#ifdef MSDOS
#ifdef _Windows
{	"ABOUT-XLISP-STAT",		S, xsabout_xlisp_stat		},
{	"MSW-CUT",			S, msw_cut			},
{	"MSW-COPY",			S, msw_copy			},
{	"MSW-PASTE",			S, msw_paste			},
{	"MSW-CLEAR",			S, msw_clear			},
{	"MSW-COPY-PASTE",		S, msw_copy_paste		},
{	"MSW-TILE",			S, msw_tile			},
{	"MSW-CASCADE",			S, msw_cascade			},
{	"MSW-CLOSE-ALL",		S, msw_closeall			},
{	"MSW-ARRANGE-ICONS",		S, msw_arrange_icons		},
{	"MSW-EXIT",			S, msw_exit			},
{	"MSW-FREE-MEM",			S, msw_free_mem			},
{	"MSW-CURSOR-SIZE",		S, msw_cursor_size		},
{	"SYSTEM",			SM, msw_win_exec		},
{	"MSW-WIN-EXEC",			SM, msw_win_exec		},
{	"MSW-WIN-HELP",			S, msw_win_help			},
{	"OPEN-FILE-DIALOG",		S, xsopenfiledialog		},
{	"SET-FILE-DIALOG",		S, xssetfiledialog		},
{	"SYSTEM::DDE-NAME-SERVICE",	S, dde_name_service		},
{	"DDE-SERVICES",			S, dde_services			},
{	"DDE-CONNECT",			S, dde_connect			},
{	"DDE-CLIENT-TRANSACTION",	S, dde_client_transaction	},
{	"DDE-DISCONNECT",		S, dde_disconnect		},
{	"MSW-GET-PROFILE-STRING",	S, msw_get_profile_string	},
{	"MSW-WRITE-PROFILE-STRING",	S, msw_write_profile_string	},
{	"MSW-MAIN-FRAME-VISIBLE",	S, msw_main_frame_visible	},
#endif /* _Windows */
#endif /* MSDOS */

/* experimental.c */
{	"SYSTEM::GETENV",		S, xsgetenv			},

/* common.c */
{	"STRING-SEARCH",		S, xsstringsearch		},
{	"COMPLEX",			S, xsrcomplex			},
{	"CONJUGATE",			S, xsrconjugate			},
{	"REALPART",			S, xsrrealpart			},
{	"IMAGPART",			S, xsrimagpart			},

/* distributions.c */
{	"NORMAL-CDF",			S, xsrnormalcdf			},
{	"BETA-CDF",			S, xsrbetacdf			},
{	"GAMMA-CDF",			S, xsrgammacdf			},
{	"CHISQ-CDF",			S, xsrchisqcdf			},
{	"T-CDF",			S, xsrtcdf			},
{	"F-CDF",			S, xsrfcdf			},
{	"CAUCHY-CDF",			S, xsrcauchycdf			},
{	"LOG-GAMMA",			S, xsrloggamma			},
{	"BIVNORM-CDF",			S, xsrbnormcdf			},
{	"NORMAL-QUANT",			S, xsrnormalquant		},
{	"CAUCHY-QUANT",			S, xsrcauchyquant		},
{	"BETA-QUANT",			S, xsrbetaquant			},
{	"GAMMA-QUANT",			S, xsrgammaquant		},
{	"CHISQ-QUANT",			S, xsrchisqquant		},
{	"T-QUANT",			S, xsrtquant			},
{	"F-QUANT",			S, xsrfquant			},
{	"NORMAL-DENS",			S, xsrnormaldens		},
{	"CAUCHY-DENS",			S, xsrcauchydens		},
{	"BETA-DENS",			S, xsrbetadens			},
{	"GAMMA-DENS",			S, xsrgammadens			},
{	"CHISQ-DENS",			S, xsrchisqdens			},
{	"T-DENS",			S, xsrtdens			},
{	"F-DENS",			S, xsrfdens			},
{	"UNIFORM-RAND",			S, xsruniformrand		},
{	"NORMAL-RAND",			S, xsrnormalrand		},
{	"CAUCHY-RAND",			S, xsrcauchyrand		},
{	"GAMMA-RAND",			S, xsrgammarand			},
{	"CHISQ-RAND",			S, xsrchisqrand			},
{	"T-RAND",			S, xsrtrand			},
{	"BETA-RAND",			S, xsrbetarand			},
{	"F-RAND",			S, xsrfrand			},

/* ddistributions.c */
{	"BINOMIAL-CDF",			S, xsrbinomialcdf		},
{	"POISSON-CDF",			S, xsrpoissoncdf		},
{	"BINOMIAL-PMF",			S, xsrbinomialpmf		},
{	"POISSON-PMF",			S, xsrpoissonpmf		},
{	"BINOMIAL-QUANT",		S, xsrbinomialquant		},
{	"POISSON-QUANT",		S, xsrpoissonquant		},
{	"BINOMIAL-RAND",		S, xsrbinomialrand		},
{	"POISSON-RAND",			S, xsrpoissonrand		},

/* linalg.c */
{	"ANY-COMPLEX-ELEMENTS",		S, xsanycomplex			},
{	"TRANSPOSE-INTO",		S, xstransposeinto		},
{	"GENERIC-TO-LINALG",		S, xsgen2linalg			},
{	"LINALG-TO-GENERIC",		S, xslinalg2gen			},
{	"LINPACK-DGECO",		S, xslpdgeco			},
{	"LINPACK-DGEDI",		S, xslpdgedi			},
{	"LINPACK-DGEFA",		S, xslpdgefa			},
{	"LINPACK-DGESL",		S, xslpdgesl			},
{	"LINPACK-ZGECO",		S, xslpzgeco			},
{	"LINPACK-ZGEDI",		S, xslpzgedi			},
{	"LINPACK-ZGEFA",		S, xslpzgefa			},
{	"LINPACK-ZGESL",		S, xslpzgesl			},
{	"LINPACK-DSVDC",		S, xslpdsvdc			},
{	"LINPACK-ZSVDC",		S, xslpzsvdc			},
{	"LINPACK-DQRDC",		S, xslpdqrdc			},
{	"LINPACK-DQRSL",		S, xslpdqrsl			},
{	"LINPACK-ZQRDC",		S, xslpzqrdc			},
{	"LINPACK-ZQRSL",		S, xslpzqrsl			},
{	"EISPACK-CH",			S, xseispackch			},
{	"EISPACK-RS",			S, xseispackrs			},
{	"CHOL-DECOMP",			S, xschol_decomp		},
{	"MAKE-ROTATION",		S, xsmake_rotation		},
{	"GET-SMOOTHER-DATA",		SM, xsgetsmdata			},
{	"BASE-SPLINE",			S, xsbasespline			},
{	"BASE-KERNEL-SMOOTH",		S, xsbasekernelsmooth		},
{	"BASE-LOWESS",			S, xsbaselowess			},
{	"SURFACE-CONTOUR",		S, xssurface_contour		},
{	"FFT",				S, xsfft			},
{	"AX+Y",				S, xsaxpy			},

/* matrices1.c */
{	"DIAGONAL",			S, xsdiagonal			},
{	"ROW-LIST",			S, xsrowlist			},
{	"COLUMN-LIST",			S, xscolumnlist			},
{	"BIND-ROWS",			S, xsbindrows			},
{	"BIND-COLUMNS",			S, xsbindcols			},
{	"TRANSPOSE-LIST",		S, xstransposelist		},

/* matrices2.c */
{	"BASE-MAKE-SWEEP-MATRIX",	S, xsbasemkswpmat		},
{	"SWEEP-IN-PLACE",		S, xssweepinplace		},

/* basics.c */
{	"SEQUENCEP",			S, xssequencep			},
{	"COPY-VECTOR",			S, xscopyvector			},
{	"COPY-ARRAY",			S, xscopyarray			},
{	"SPLIT-LIST",			S, xssplitlist			},
{	"WHICH",			S, xswhich			},
{	"ISEQ",				S, xsiseq			},
{	"REPEAT",			S, xsrepeat			},
{	"SELECT",			S, xsselect			},
{	"SET-SELECT",			S, xssetselect			},
{	"PERMUTE-ARRAY",		S, xspermutearray		},

/* compound.c */
{	"COMPOUND-DATA-P",		S, xscompoundp			},
{	"COMPOUND-DATA-LENGTH",		S, xscompound_length		},
{	"COMPOUND-DATA-SEQ",		S, xscompound_seq		},
{	"MAP-ELEMENTS",			S, xsmap_elements		},

/* math.c */
{	"+",				S, xsradd			},
{	"-",				S, xsrsub			},
{	"*",				S, xsrmul			},
#ifdef BIGNUMS
{	"/",				S, xsrfdiv			},
#else
{	"/",				S, xsrdiv			},
#endif /* BIGNUMS */
{	"DIVIDE",			S, xsrdiv			},
{	"PMIN",				S, xsrmin			},
{	"PMAX",				S, xsrmax			},
{	"REM",				S, xsrrem			},
{	"MOD",				S, xsrmod			},
#ifdef BIGNUM
{	"^",				S, xsrfexpt			},
{	"**",				S, xsrfexpt			},
{	"EXPT",				S, xsrfexpt			},
#else
{	"^",				S, xsrexpt			},
{	"**",				S, xsrexpt			},
{	"EXPT",				S, xsrexpt			},
#endif /* BIGNUMS */
{	"IEXPT",			S, xsrexpt			},
{	"LOG",				S, xsrlog			},
#ifdef BIGNUMS
{	"DENOMINATOR",			S, xsrdenominator		},
{	"NUMERATOR",			S, xsrnumerator			},
{	"RATIONAL",			S, xsrrational			},
#endif /* BIGNUMS */

{	"LOGAND",			S, xsrlogand			},
{	"LOGIOR",			S, xsrlogior			},
{	"LOGXOR",			S, xsrlogxor			},
{	"LOGNOT",			S, xsrlognot			},

{	"ABS",				S, xsrabs			},
{	"1+",				S, xsradd1			},
{	"1-",				S, xsrsub1			},
{	"SIN",				S, xsrsin			},
{	"COS",				S, xsrcos			},
{	"TAN",				S, xsrtan			},
{	"EXP",				S, xsrexp			},
{	"SQRT",				S, xsrsqrt			},
{	"TRUNCATE",			SM, xsrfix			},
{	"FLOAT",			S, xsrfloat			},
{	"RANDOM",			S, xsrrand			},
{	"FLOOR",			SM, xsrfloor			},
{	"CEILING",			SM, xsrceil			},
{	"ROUND",			SM, xsrround			},
{	"ASIN",				S, xsrasin			},
{	"ACOS",				S, xsracos			},
{	"ATAN",				S, xsratan			},
{	"PHASE",			S, xsrphase			},

{	"MINUSP",			S, xsrminusp			},
{	"ZEROP",			S, xsrzerop			},
{	"PLUSP",			S, xsrplusp			},
{	"EVENP",			S, xsrevenp			},
{	"ODDP",				S, xsroddp			},

{	"<",				S, xsrlss			},
{	"<=",				S, xsrleq			},
{	"=",				S, xsrequ			},
{	"/=",				S, xsrneq			},
{	">=",				S, xsrgeq			},
{	">",				S, xsrgtr			},

/* objects.c */
{	"KIND-OF-P",			S, xskind_of_p			},
{	"SLOT-VALUE",			S, xsslot_value			},
{	"MAKE-OBJECT",			S, xsmake_object		},
{	"SEND",				SM, xmsend			},
{	"SEND-SUPER",			SM, xmsendsuper			},
{	"CALL-NEXT-METHOD",		SM, xscall_next			},
{	"CALL-METHOD",			SM, xscall_method		},
{	"DEFMETH",			F, xsdefmeth			},
{	"DEFPROTO",			F, xsdefproto			},

/* optimize.c */
#ifdef OPTIMIZE
{	"BRACKET-SEARCH",		S, xsbracket_search		},
{	"GOLDEN-SEARCH",		S, xsgolden_search		},
{	"PARABOLIC-SEARCH",		S, xsparabolic_search		},
#endif /* OPTIMIZE */

/* sortdata.c */
{	"SORT-DATA",			S, xssortdata			},
{	"ORDER",			S, xsorder			},
{	"RANK",				S, xsrank			},
{	"SORT-DATA<",			S, xssortcmp			},
{	"ORDER<",			S, xsordercmp			},

/* statistics.c */
{	"SUM",				S, xssum			},
{	"PROD",				S, xsprod			},
{	"MIN",				S, xsmin			},
{	"MAX",				S, xsmax			},
{	"COUNT-ELEMENTS",		S, xscount			},
{	"ELEMENT-SEQ",			S, xselement_seq		},
{	"IF-ELSE",			S, xsifelse			},
{	"MEAN",				S, xsmean			},
{	"SAMPLE",			S, xssample			},

#ifndef NOGRAPHICS
/* windows.c */
{	"SCREEN-SIZE",			S, xsscreen_size		},
{	"SCREEN-HAS-COLOR",		S, xsscreen_has_color		},
{	"SYSTEM-HAS-WINDOWS",		S, xssystem_has_windows		},
{	"FLUSH-GRAPHICS",		S, xsflush_graphics		},

/* xsiviewwindow.c */
{	"RESET-GRAPHICS-BUFFER",	S, iview_window_reset_buffer	},
{	"MAKE-COLOR",			S, gw_make_color		},
{	"FREE-COLOR",			S, gw_free_color		},
{	"MAKE-CURSOR",			S, gw_make_cursor		},
{	"FREE-CURSOR",			S, gw_free_cursor		},

/* xsiviewinternal.c */
{	"UNLINK-ALL-WINDOWS",		S, iview_unlink_all_windows	},

/* xsiview.c */
{	"GET-NICE-RANGE",		S, iview_get_nice_range		},

/* xsgraphics.c */
{	"HISTOGRAM",			S, xshistogram			},
{	"PLOT-POINTS",			S, xsplot_points		},
{	"PLOT-LINES",			S, xsplot_lines			},
{	"SPIN-PLOT",			S, xsspin_plot			},
{	"SCATTERPLOT-MATRIX",		S, xsscatterplot_matrix		},
{	"NAME-LIST",			S, xsnamelist			},
#endif /* NOGRAPHICS */
#ifdef UNIX
{	"GNU-PLOT-POINTS",		S, gnupointplot			},
{	"GNU-PLOT-LINES",	       	S, gnulineplot			},
#endif /* UNIX */

#ifdef MACINTOSH
/* macxsgraph.c */
{	"PICK-COLOR",			S, xspick_color			},

/* macdynload.c */
{	"OPEN-RESOURCE-FILE",		S, xsopen_resfile		},
{	"CLOSE-RESOURCE-FILE",		S, xsclose_resfile		},
{	"CALL-CFUN",			S, xscall_cfun			},
#endif /* MACINTOSH */

#ifdef MSDOS
#ifdef _Windows
/* mswdynld.c */
{	"LOAD-DLL",			SM, xsload_dll			},
{	"FREE-DLL",			S, xsfree_dll			},
{	"CALL-CFUN",			S, xscall_cfun			},
#endif /* _Windows */
#endif /* MSDOS */

#ifdef X11WINDOWS
{	"PARSE-COLOR",			S, xsparse_color		},
{	"BEST-CURSOR-SIZE",		S, xsbest_cursor_size		},
{	"BITMAP-FROM-FILE",		S, xsbitmap_from_file		},
{	"X11-OPTIONS",			S, xsx11_options		},
#endif /* X11WINDOWS */

#ifdef FOREIGNCALL
{	"DYN-LOAD",			S, xsdynload			},
{	"CALL-CFUN",			S, xscall_cfun			},
{	"CALL-FSUB",			S, xscall_fsub			},
{	"CALL-LFUN",			S, xscall_lfun			},
#endif /* FOREIGNCALL */
#ifdef UNIX
{	"SYSTEM",			S, xssystem			},
{	"POPEN",			S, Prim_POPEN			},
{	"PCLOSE",			S, Prim_PCLOSE			},
#endif /* UNIX */

#ifdef MSDOS
#ifndef _Windows
{	"SYSTEM",			S, xsystem			},
{	"GET-KEY",			S, xgetkey			},
#endif /* _Windows */

#ifdef NOGRAPHICS
{	"PLOT-POINTS",			S, gnupointplot			},
{	"PLOT-LINES",			S, gnulineplot			},
#endif /* NOGRAPHICS */
#endif /* MSDOS */
#ifdef BYTECODE
{	"MAKE-BYTE-CODE",		S, xlmakebcode			},
{	"BYTE-CODE-CLOSE",		S, xlbcclose			},
{	"DYNAMIC-VALUE",		S, xldval			},
{	"GET-LAMBDA-NAME",		S, xlgetlambdaname		},
{	"COERCE-TO-MACRO",		S, xlcoercemacro		},
{	"MAKE-CPS-NODE",		S, xlmakecpsnode		},
{	"CPS-NODE-INTERNAL",		S, xlcpsinternal		},
{	"CPS-NODE-TRANSFORM",		S, xlcpstransform		},
{	"CPS-LEAF-NODE-P",		S, xlcpsleafnodep		},
{	"CPS-LAMBDA-NODE-P",		S, xlcpslambdanodep		},
{	"CPS-CALL-NODE-P",		S, xlcpscallnodep		},
{	"CPS-ANY-REFERENCES-P",		S, xlcpsanyrefs			},
{	"CPS-FIND-REFERENCES",		S, xlcpsfindrefs		},
{	"CPS-NODE-CHILDREN",		S, xlcpsnodechildren		},
{	"CPS-NODE-PARENT",		S, xlcpsnodeparent		},
{	"CPS-NODE-SIMPLIFIED-P",	S, xlcpsnodesimplified		},
{	"CPS-NODE-NOTE",		S, xlcpsnodenote		},
{	"CPS-LEAF-NODE-VALUE",		S, xlcpsleafnodevalue		},
{	"CPS-LEAF-NODE-COUNT",		S, xlcpsleafnodecount		},
{	"CPS-LAMBDA-NODE-ARGLIST",	S, xlcpslambdanodearglist	},
{	"CPS-LAMBDA-NODE-LAMBDA-LIST",	S, xlcpslambdanodelambdalist	},
{	"CPS-LAMBDA-NODE-NAME",		S, xlcpslambdanodename		},
{	"CPS-SET-NODE-CHILDREN",	S, xlcpssetnodechildren		},
{	"CPS-SET-NODE-PARENT",		S, xlcpssetnodeparent		},
{	"CPS-SET-NODE-SIMPLIFIED",	S, xlcpssetnodesimplified	},
{	"CPS-SET-NODE-NOTE",		S, xlcpssetnodenote		},
{	"CPS-SET-LEAF-NODE-VALUE",	S, xlcpssetleafnodevalue	},
{	"CPS-SET-LEAF-NODE-COUNT",	S, xlcpssetleafnodecount	},
{	"CPS-SET-LAMBDA-NODE-ARGLIST",	S, xlcpssetlambdanodearglist	},
{	"CPS-SET-LAMBDA-NODE-LAMBDA-LIST",S, xlcpssetlambdanodelambdalist},
{	"CPS-SET-LAMBDA-NODE-NAME",	S, xlcpssetlambdanodename	},
{	"CPS-LAMBDA-NODE-BODY",		S, xlcpslambdanodebody		},
{	"CPS-CALL-NODE-FUNCTION",	S, xlcpscallnodefunction	},
{	"CPS-CALL-NODE-ARGS",		S, xlcpscallnodeargs		},
#endif /* BYTECODE */

{	"BLAS-DASUM",		S,	xblasdasum		},
{	"BLAS-DAXPY",		S,	xblasdaxpy		},
{	"BLAS-DCOPY",		S,	xblasdcopy		},
{	"BLAS-DDOT",		S,	xblasddot		},
{	"BLAS-DNRM2",		S,	xblasdnrm2		},
{	"BLAS-DROT",		S,	xblasdrot		},
{	"BLAS-DROTG",		SM,	xblasdrotg		},
{	"BLAS-DSCAL",		S,	xblasdscal		},
{	"BLAS-DSWAP",		S,	xblasdswap		},
{	"BLAS-IDAMAX",		S,	xblasidamax		},
{	"BLAS-IZAMAX",		S,	xblasizamax		},
{	"BLAS-DZASUM",		S,	xblasdzasum		},
{	"BLAS-DZNRM2",		S,	xblasdznrm2		},
{	"BLAS-ZAXPY",		S,	xblaszaxpy		},
{	"BLAS-ZCOPY",		S,	xblaszcopy		},
{	"BLAS-ZDOTC",		S,	xlbaszdotc		},
{	"BLAS-ZDOTU",		S,	xlbaszdotu		},
{	"BLAS-ZDROT",		S,	xlbaszdrot		},
{	"BLAS-ZDSCAL",		S,	xlbaszdscal		},
{	"BLAS-ZROTG",		SM,	xlbaszrotg		},
{	"BLAS-ZSCAL",		S,	xlbaszscal		},
{	"BLAS-ZSWAP",		S,	xlbaszswap		},
{	"BLAS-DGEMV",		S,	xlblasdgemv		},
{	"BLAS-DTRMV",		S,	xlblasdtrmv		},
{	"BLAS-DGER",		S,	xlblasdger		},
{	"BLAS-DTRSV",		S,	xlblasdtrsv		},
{	"BLAS-ZGEMV",		S,	xlblaszgemv		},
{	"BLAS-ZTRMV",		S,	xlblasztrmv		},
{	"BLAS-ZGERC",		S,	xlblaszgerc		},
{	"BLAS-ZGERU",		S,	xlblaszgeru		},
{	"BLAS-ZTRSV",		S,	xlblasztrsv		},
{	"BLAS-DGEMM",		S,	xlblasdgemm		},
{	"BLAS-DTRSM",		S,	xlblasdtrsm		},
{	"BLAS-ZGEMM",		S,	xlblaszgemm		},
{	"BLAS-ZTRSM",		S,	xlblasztrsm		},
{	"UNCMIN-MAXRELSIZE",	S,	xsminmaxrelsize		},
{	"UNCMIN-CHOLSOLVE",	S,	xsmincholsolve		},
{	"UNCMIN-MODELHESS",	S,	xsminmodelhess		},
{	"UNCMIN-GRADSIZE",	S,	xsmingradsize		},
{	"UNCMIN-LINESEARCH",	S,	xsminlinesearch		},
{	"UNCMIN-MAXRELSIZE",	S,	xsminmaxrelsize		},
{	"UNCMIN-CHOLSOLVE",	S,	xsmincholsolve		},
{	"UNCMIN-MODELHESS",	S,	xsminmodelhess		},
{	"UNCMIN-GRADSIZE",	S,	xsmingradsize		},
{	"UNCMIN-LINESEARCH",	S,	xsminlinesearch		},
#ifdef _Windows
{	"MSW-PRINT",		S,	msw_print		},
{	"MSW-PAGESETUP",	S,	msw_pagesetup		},
#endif
#else
{   "SYSTEM",   S,  xsystem },

#if !(defined(UNIX)||defined(AMIGA)||defined(__SASC__))
{   "GET-KEY",  S,  xgetkey },
#endif

#ifdef GRAPHICS
{   "CLS",      S,  xcls    },
{   "GOTO-XY",  S,  xgotoxy },
{   "CLEOL",    S,  xcleol  },
{   "MODE",     S,  xmode   },
{   "COLOR",    S,  xcolor  },
{   "MOVE",     S,  xmove   },
{   "DRAW",     S,  xdraw   },
{   "MOVEREL",  S,  xmoverel},
{   "DRAWREL",  S,  xdrawrel},
#endif

#ifdef UNIX
#ifdef FILETABLE
{   "POPEN",    S,  Prim_POPEN},
{   "PCLOSE",   S,  Prim_PCLOSE},
#endif /* FILETABLE */
#endif
#endif /* XLISP_STAT */
