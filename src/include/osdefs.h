#ifdef XLISP_STAT
/* menus.c */
extern LVAL xsmenu_isnew(V), xsallocate_menu(V), xsdispose_menu(V),
  xsinstall_menu(V), xsremove_menu(V), xsupdate_menu(V), xsallocated_p(V),
  xsmenu_title(V), xsmenu_items(V), xsinstalled_p(V), xsappend_items(V),
  xsdelete_items(V), xsmenu_select(V), xsmenu_popup(V), xsmenu_enabled(V);

#ifdef MACINTOSH
extern LVAL xsapple_menu_isnew(V), xsapple_menu_select(V);
#endif /* MACINTOSH */

extern LVAL xsitem_isnew(V), xsitem_title(V), xsitem_key(V), xsitem_style(V),
  xsitem_mark(V), xsitem_installed_p(V), xsitem_update(V), xsitem_action(V),
  xsitem_do_action(V), xsitem_enabled(V);
  
#ifdef MACINTOSH
extern LVAL xsabout_xlisp_stat(V);
extern LVAL xlaunchapp(V);
extern LVAL xstrtoostype(V);
extern LVAL xgetfrontprocess(V);
extern LVAL xsetfrontprocess(V);
extern LVAL xgetprocesslist(V);
extern LVAL xgettarget(V);
extern LVAL xgettargetlist(V);
extern LVAL xbrowsetargets(V);
extern LVAL xsendappleevent(V);
extern LVAL xppcstartup(V);
#endif /* MACINTOSH */
#ifdef MSDOS
#ifdef _Windows
extern LVAL xsabout_xlisp_stat(V);
extern LVAL msw_cut(V), msw_copy(V), msw_paste(V), msw_clear(V),
  msw_copy_paste(V), msw_tile(V), msw_cascade(V), msw_closeall(V),
  msw_arrange_icons(V), msw_exit(V), msw_win_exec(V), msw_win_help(V),
  msw_free_mem(V), msw_cursor_size(V), msw_print(V), msw_pagesetup(V);
extern LVAL dde_connect(V), dde_client_transaction(V), dde_disconnect(V);
extern LVAL dde_name_service(V), dde_services(V);
extern LVAL xsopenfiledialog(V), xssetfiledialog(V);
extern LVAL msw_get_profile_string(V), msw_write_profile_string(V);
extern LVAL msw_main_frame_visible(V);
#endif /* _Windows */
#endif /* MSDOS */

/* dialog.c */
extern LVAL xsdialog_isnew(V), xsdialog_allocate(V), xsdialog_remove(V),
  xsdialog_allocated_p(V), xsdialog_default_button(V), xsdialog_modal(V),
  xsdialog_item_do_action(V), xsdialog_item_action(V), xsbutton_item_isnew(V),
  xstoggle_item_isnew(V), xstoggle_item_value(V), xstext_item_isnew(V),
  xstext_item_text(V), xschoice_item_isnew(V), xschoice_item_value(V),
  xsscroll_item_isnew(V), xsscroll_item_value(V), xsscroll_item_max(V),
  xsscroll_item_min(V), xsscroll_item_action(V),
  xslist_item_isnew(V), xslist_item_action(V), xslist_item_text(V),
  xslist_item_selection(V);
extern LVAL xssysbeep(V);

#ifdef MACINTOSH
/* edit.c */
extern LVAL xsopenfiledialog(V), xssetfiledialog(V), xsfrontwindow(V), xshidefrontwindow(V),
  xssystem_edit(V), xssetvolume(V);
extern LVAL xslistener_isnew(V), xslistener_allocate(V);
extern LVAL xsedit_window_isnew(V), xsedit_window_allocate(V), xsedit_window_cut(V),
  xsedit_window_copy(V),
  xsedit_window_paste(V), xsedit_window_revert(V), xsedit_window_save(V), 
  xsedit_window_save_as(V), xsedit_window_save_copy(V),
  xsedit_window_selection_stream(V), xsedit_window_paste_stream(V),
  xsedit_window_flush_window(V), xsedit_window_paste_string(V),
  xsedit_window_remove(V), xsedit_window_activate(V), xsedit_window_update(V),
  xsedit_window_find(V);
#endif /* MACINTOSH */

/* experimental.c */
extern LVAL xsgetenv(V);

/* common.c */
extern LVAL xsstringsearch(V), xsrcomplex(V), xsrconjugate(V),
  xsrrealpart(V), xsrimagpart(V);

/* commonmath.c */
extern LVAL xlog(V), xfloor(V), xceil(V), xround(V), xasin(V), xacos(V),
  xatan(V);

/* basics.c */
extern LVAL xssequencep(V), xscopyvector(V), xscopyarray(V), xssplitlist(V),
  xswhich(V), xsiseq(V), xsrepeat(V), xsselect(V), xssetselect(V),
  xspermutearray(V);

/* compound.c */
extern LVAL xscompoundp(V), xscompound_length(V), xscompound_seq(V),
  xsmap_elements(V);

/* distributions.c */
extern LVAL xsrnormalcdf(V), xsrbetacdf(V), xsrgammacdf(V), xsrchisqcdf(V),
  xsrtcdf(V), xsrfcdf(V), xsrcauchycdf(V), xsrloggamma(V), xsrbnormcdf(V),
  xsrnormalquant(V), xsrcauchyquant(V), xsrbetaquant(V), xsrgammaquant(V), 
  xsrchisqquant(V), xsrtquant(V), xsrfquant(V), xsruniformrand(V),
  xsrnormalrand(V), xsrcauchyrand(V), xsrgammarand(V), xsrchisqrand(V),
  xsrtrand(V), xsrbetarand(V), xsrfrand(V),
  xsrnormaldens(V), xsrcauchydens(V), xsrbetadens(V), xsrgammadens(V),
  xsrchisqdens(V), xsrtdens(V), xsrfdens(V), xsruniformrand(V);

/* ddistributions.c */
extern LVAL xsrbinomialcdf(V), xsrpoissoncdf(V), xsrbinomialpmf(V), xsrpoissonpmf(V),
  xsrbinomialquant(V), xsrpoissonquant(V), xsrbinomialrand(V), xsrpoissonrand(V);

/* linalg.c */
extern LVAL xsanycomplex(V), xstransposeinto(V),
  xsgen2linalg(V), xslinalg2gen(V);
extern LVAL xslpdgeco(V), xslpdgedi(V), xslpdgefa(V), xslpdgesl(V);
extern LVAL xslpzgeco(V), xslpzgedi(V), xslpzgefa(V), xslpzgesl(V);
extern LVAL xslpdsvdc(V), xslpzsvdc(V);
extern LVAL xslpdqrdc(V), xslpdqrsl(V), xslpzqrdc(V), xslpzqrsl(V);
extern LVAL xseispackch(V), xseispackrs(V);
extern LVAL xsgetsmdata(V), xsbasespline(V), xsbasekernelsmooth(V);
extern LVAL xsbaselowess(V);
extern LVAL xschol_decomp(V),
  xsmake_rotation(V), xsspline(V), xskernel_dens(V), xskernel_smooth(V), 
  xssurface_contour(V), xsfft(V), xsaxpy(V);

/* mats1.c */
extern LVAL xsmatmult(V), xscrossproduct(V), xsdiagonal(V),
  xsrowlist(V), xscolumnlist(V), xsbindrows(V), xsbindcols(V),
  xstransposelist(V);

/* mats2.c */
extern LVAL xsbasemkswpmat(V), xssweepinplace(V);
extern LVAL xsmakesweepmatrix(V), xssweepoperator(V);

/* math.c */
extern LVAL xsradd(V), xsrsub(V), xsrmul(V), xsrdiv(V),
  xsrrem(V), xsrmod(V), xsrmin(V), xsrmax(V), xsrexpt(V), xsrlog(V);
#ifdef BIGNUMS
extern LVAL xsrfdiv(V), xsrfexpt(V);
extern LVAL xsrdenominator(V), xsrnumerator(V), xsrrational(V);
#endif /* BIGNUMS */

extern LVAL xsrlogand(V), xsrlogior(V), xsrlogxor(V), xsrlognot(V);

extern LVAL xsrabs(V), xsradd1(V), xsrsub1(V), xsrsin(V), xsrcos(V), xsrtan(V),
  xsrexp(V), xsrsqrt(V), xsrfix(V), xsrfloat(V), xsrrand(V), xsrfloor(V), xsrceil(V),
  xsrround(V), xsrasin(V), xsracos(V), xsratan(V), xsrphase(V);

extern LVAL xsrminusp(V), xsrzerop(V), xsrplusp(V), xsrevenp(V), xsroddp(V);

extern LVAL xsrlss(V), xsrleq(V), xsrequ(V), xsrneq(V), xsrgeq(V), xsrgtr(V);

/* objects.c */
extern LVAL xskind_of_p(V), xsslot_value(V), xsobject_null_method(V);
extern LVAL xsmake_object(V), xsreparent_object(V), xshas_slot(V), xsadd_slot(V),
  xsdelete_slot(V), xsmslot_value(V), xshas_method(V), xsadd_method(V),
  xsdelete_method(V), xsmessage_method(V), xmsend(V), xscall_method(V), xscall_next(V),
  xsshow_object(V), xmsendsuper(V), xsobject_isnew(V), xsparents(V),
  xsprecedence_list(V), xsobject_slots(V), xsobject_methods(V), xsdefmeth(V),
  xsobject_documentation(V), xsdefproto(V), xsmakeproto(V);
  
/* optimize.c */
#ifdef OPTIMIZE
extern LVAL xsbracket_search(V), xsgolden_search(V), xsparabolic_search(V);
#endif /* OPTIMIZE */

/* sortdata.c */
extern LVAL xssortdata(V), xsorder(V), xsrank(V);

/* statistics.c */
extern LVAL xssum(V), xsprod(V), xsmin(V), xsmax(V), xscount(V),
  xselement_seq(V), xsifelse(V), xsmean(V), xssample(V);

/* windows.c */
extern LVAL xsshowwindow(V), xshidewindow(V);
extern LVAL  xsscreen_size(V), xsscreen_has_color(V), xssystem_has_windows(V),
  xswindow_title(V), xswindow_location(V), xswindow_size(V),
  xswindow_frame_location(V), xswindow_frame_size(V), xsflush_graphics(V);

/* xsiviewwindow.c */
extern LVAL iview_window_isnew(V), iview_window_allocate(V);
extern LVAL xsiview_window_update(V), xsiview_window_activate(V);
extern LVAL iview_window_idle_on(V);
extern LVAL iview_window_menu(V);
extern LVAL iview_window_remove(V), iview_window_while_button_down(V),
  iview_window_show_window(V);
extern LVAL iview_window_canvas_width(V), iview_window_canvas_height(V),
  iview_window_line_type(V), iview_window_draw_mode(V),
  iview_window_draw_color(V), iview_window_back_color(V), iview_window_use_color(V),
  iview_window_reverse_colors(V), iview_window_view_rect(V),
  iview_window_line_width(V);
extern LVAL iview_window_has_h_scroll(V), iview_window_has_v_scroll(V),
  iview_window_scroll(V), iview_window_h_scroll_incs(V),
  iview_window_v_scroll_incs(V);
extern LVAL iview_window_draw_line(V), iview_window_draw_point(V),
  iview_window_erase_rect(V),
  iview_window_frame_rect(V), iview_window_paint_rect(V),
  iview_window_erase_oval(V), iview_window_frame_oval(V),
  iview_window_paint_oval(V), iview_window_erase_arc(V),
  iview_window_frame_arc(V), iview_window_paint_arc(V),
  iview_window_erase_poly(V), iview_window_frame_poly(V),
  iview_window_paint_poly(V);
extern LVAL iview_window_text_ascent(V), iview_window_text_descent(V), 
  iview_window_text_width(V),
  iview_window_draw_string(V), iview_window_draw_string_up(V),
  iview_window_draw_text(V), iview_window_draw_text_up(V);
extern LVAL iview_window_draw_symbol(V), iview_window_replace_symbol(V);
extern LVAL iview_window_start_buffering(V), iview_window_buffer_to_screen(V);
extern LVAL iview_window_clip_rect(V), iview_window_cursor(V),
  iview_window_reset_buffer(V);
extern LVAL iview_window_dump_image(V);
extern LVAL gw_make_color(V), gw_free_color(V), gw_make_cursor(V),
  gw_free_cursor(V), gw_draw_bitmap(V);

/* xsiviewinternal.c */
extern LVAL iview_isnew(V), iview_allocate(V);
extern LVAL iview_content_rect(V), iview_content_origin(V),
  iview_content_variables(V), iview_click_range(V), iview_mouse_mode(V),
  iview_showing_labels(V), iview_margin(V), iview_fixed_aspect(V), iview_dirty(V);
extern LVAL iview_x_axis(V), iview_y_axis(V);
extern LVAL iview_brush(V), iview_erase_brush(V), iview_draw_brush(V),
  iview_move_brush(V), iview_resize_brush(V);
extern LVAL iview_std_click(V), iview_std_motion(V), iview_do_click(V),
  iview_do_motion(V), iview_unselect_all_points(V),
  iview_erase_selection(V), iview_mask_selection(V),
  iview_unmask_all_points(V), iview_show_all_points(V),
  iview_all_points_showing(V), iview_all_points_unmasked(V),
  iview_any_points_selected(V);
extern LVAL iview_linked(V), iview_links(V), iview_unlink_all_windows(V);

/* xsiview.c */
extern LVAL iview_num_variables(V), iview_variable_label(V), iview_range(V),
  iview_scaled_range(V), iview_screen_range(V), iview_transformation(V),
  iview_apply_transformation(V);

extern LVAL iview_add_points(V), iview_clear_points(V), iview_num_points(V), 
  iview_point_coordinate(V), iview_point_screen_coordinate(V),
  iview_point_transformed_coordinate(V),
  iview_point_masked(V), iview_point_color(V), iview_point_state(V),
  iview_point_screen_state(V), iview_point_marked(V),
  iview_point_label(V), iview_point_symbol(V);
  
extern LVAL iview_add_lines(V), iview_clear_lines(V), iview_num_lines(V),
  iview_line_coordinate(V), iview_line_screen_coordinate(V),
  iview_line_transformed_coordinate(V),
  iview_line_masked(V), iview_line_color(V), iview_line_next(V),
  iview_line_type(V), iview_line_width(V);

#ifdef USESTRINGS
extern LVAL iview_add_strings(V), iview_clear_strings(V), iview_num_strings(V),
  iview_string_coordinate(V), iview_string_screen_coordinate(V),
  iview_string_transformed_coordinate(V),
  iview_string_masked(V), iview_string_color(V), iview_string_modifiers(V);
#endif /* USESTRINGS */

extern LVAL iview_draw_data_points(V), iview_draw_data_lines(V);
#ifdef USESTRINGS
extern LVAL iview_draw_data_strings(V);
#endif /* USESTRINGS */

extern LVAL iview_std_resize(V), iview_std_redraw(V),
  iview_std_redraw_background(V), iview_std_redraw_content(V), 
  iview_std_clear_content(V),
  iview_std_adjust_screen(V), iview_std_adjust_points_in_rect(V),
  iview_std_adjust_screen_point(V), iview_std_mark_points_in_rect(V);

extern LVAL iview_rotate_2(V);
extern LVAL iview_get_nice_range(V);
extern LVAL iview_adjust_depth_cuing(V);

/* xsiviewcustom.c */
extern LVAL iview_spin_allocate(V),
  iview_spin_content_variables(V), 
  iview_spin_showing_axes(V), iview_spin_depth_cuing(V),
  iview_spin_resize(V), 
  iview_spin_angle(V), iview_spin_rotate(V),
  iview_spin_redraw_content(V);
extern LVAL iview_scatmat_allocate(V), iview_scatmat_resize(V),
  iview_scatmat_redraw_content(V),iview_scatmat_click(V), 
  iview_scatmat_motion(V),
  iview_scatmat_add_points(V), iview_scatmat_add_lines(V),
  iview_scatmat_adjust_screen_point(V), iview_scatmat_adjust_points_in_rect(V),
  iview_scatmat_mark_points_in_rect(V);
extern LVAL iview_list_allocate(V), iview_list_redraw_content(V), 
  iview_list_add_points(V), iview_list_adjust_screen_point(V),
  iview_list_adjust_points_in_rect(V), iview_list_mark_points_in_rect(V);
extern LVAL iview_hist_isnew(V), iview_hist_allocate(V), iview_hist_add_points(V),
  iview_hist_clear_points(V), iview_hist_resize(V), 
  iview_hist_redraw_content(V), iview_hist_adjust_screen(V),
  iview_hist_num_bins(V), iview_hist_bin_counts(V),
  iview_hist_adjust_screen_point(V), iview_hist_adjust_points_in_rect(V),
  iview_hist_mark_points_in_rect(V);
extern LVAL iview_plot2d_add_points(V), iview_plot2d_add_lines(V);
#ifdef USESTRINGS
extern LVAL iview_scatmat_add_strings(V), iview_plot2d_add_strings(V);
#endif /* USESTRINGS */
  
/* xsgraphics.c */
extern LVAL iview_point_selected(V), iview_point_hilited(V),
  iview_point_showing(V),
  iview_hist_adjust_to_data(V), iview_plot2d_adjust_to_data(V),
  iview_adjust_to_data(V), iview_spin_draw_axes(V);
#ifdef MACINTOSH
extern LVAL iview_window_copy_to_clip(V);
#endif /* MACINTOSH */
extern LVAL xshistogram(V), xsplot_points(V), xsplot_lines(V), xsspin_plot(V),
  xsscatterplot_matrix(V), xsnamelist(V);
extern LVAL iview_visible_range(V), iview_scale_to_range(V), iview_scale(V),
  iview_shift(V), iview_clear_masks(V), iview_slice_variable(V), 
  iview_real_to_screen(V), iview_screen_to_real(V), iview_scaled_to_screen(V), 
  iview_screen_to_scaled(V), iview_points_in_rect(V),
  iview_window_drag_grey_rect(V), iview_points_showing(V),
  iview_points_hilited(V), iview_points_selected(V),
  iview_cycle_selection_symbols(V);

#ifdef MACINTOSH
/* macxsgraph.c */
extern LVAL xspick_color(V);

/* macdynload.c */
extern LVAL xsopen_resfile(V), xsclose_resfile(V), xscall_cfun(V);
#endif /* MACINTOSH */

#ifdef MSDOS
#ifdef _Windows
extern LVAL xsload_dll(V), xsfree_dll(V), xscall_cfun(V);
#endif /* _Windows */
#endif /* MSDOS */

#ifdef X11WINDOWS
extern LVAL xsparse_color(V), xsbest_cursor_size(V), xsbitmap_from_file(V),
  xsx11_options(V);
#endif /* X11WINDOWS */

#ifdef UNIX
extern LVAL gnupointplot(V), gnulineplot(V);
#endif /* UNIX */
#ifdef FOREIGNCALL
extern LVAL xsdynload(V), xscall_cfun(V), xscall_fsub(V), xscall_lfun(V);
#endif /* FOREIGNCALL */
#ifdef UNIX
extern LVAL xssystem(V);
extern LVAL Prim_POPEN(V), Prim_PCLOSE(V);
#endif /* UNIX */

#ifdef FILETABLE
extern int getslot _((void));
#endif

#ifdef MSDOS
/* xsgraphics.c */
extern LVAL gnupointplot(V), gnulineplot(V);

/* msstuff.c */
extern LVAL xsystem(V),xgetkey(V);
#endif /* MSDOS */
#ifdef BYTECODE
extern LVAL xlmakebcode(V), xlbcclose(V), xldval(V), xlcoercemacro(V),
  xlgetlambdaname(V);
extern LVAL xlmakecpsnode(V), xlcpsinternal(V),
  xlcpstransform(V), xlcpsleafnodep(V), xlcpslambdanodep(V),
  xlcpscallnodep(V), xlcpsanyrefs(V), xlcpsfindrefs(V),
  xlcpsnodechildren(V), xlcpsnodeparent(V), xlcpsnodesimplified(V),
  xlcpsnodenote(V), xlcpsleafnodevalue(V), xlcpsleafnodecount(V),
  xlcpslambdanodearglist(V), xlcpslambdanodelambdalist(V), xlcpslambdanodename(V),
  xlcpssetnodechildren(V), xlcpssetnodeparent(V),
  xlcpssetnodesimplified(V), xlcpssetnodenote(V), xlcpssetleafnodevalue(V),
  xlcpssetleafnodecount(V), xlcpssetlambdanodearglist(V),
  xlcpssetlambdanodelambdalist(V), xlcpssetlambdanodename(V),
  xlcpslambdanodebody(V), xlcpscallnodefunction(V), xlcpscallnodeargs(V);
#endif /* BYTECODE */

extern LVAL xblasdasum(V);
extern LVAL xblasdaxpy(V);
extern LVAL xblasdcopy(V);
extern LVAL xblasddot(V);
extern LVAL xblasdnrm2(V);
extern LVAL xblasdrot(V);
extern LVAL xblasdrotg(V);
extern LVAL xblasdscal(V);
extern LVAL xblasdswap(V);
extern LVAL xblasidamax(V);
extern LVAL xblasizamax(V);
extern LVAL xblasdzasum(V);
extern LVAL xblasdznrm2(V);
extern LVAL xblaszaxpy(V);
extern LVAL xblaszcopy(V);
extern LVAL xlbaszdotc(V);
extern LVAL xlbaszdotu(V);
extern LVAL xlbaszdrot(V);
extern LVAL xlbaszdscal(V);
extern LVAL xlbaszrotg(V);
extern LVAL xlbaszscal(V);
extern LVAL xlbaszswap(V);
extern LVAL xlblasdgemv(V);
extern LVAL xlblasdtrmv(V);
extern LVAL xlblasdger(V);
extern LVAL xlblasdtrsv(V);
extern LVAL xlblaszgemv(V);
extern LVAL xlblasztrmv(V);
extern LVAL xlblaszgerc(V);
extern LVAL xlblaszgeru(V);
extern LVAL xlblasztrsv(V);
extern LVAL xlblasdgemm(V);
extern LVAL xlblasdtrsm(V);
extern LVAL xlblaszgemm(V);
extern LVAL xlblasztrsm(V);
extern LVAL xsminmaxrelsize(V);
extern LVAL xsmincholsolve(V);
extern LVAL xsminmodelhess(V);
extern LVAL xsmingradsize(V);
extern LVAL xsminlinesearch(V);
extern LVAL xsminmaxrelsize(V);
extern LVAL xsmincholsolve(V);
extern LVAL xsminmodelhess(V);
extern LVAL xsmingradsize(V);
extern LVAL xsminlinesearch(V);
extern LVAL xssortcmp(V);
extern LVAL xsordercmp(V);
#else
extern LVAL xsystem(V);

#if !(defined(UNIX)||defined(AMIGA)||defined(__SASC__))
extern LVAL xgetkey(V);
#endif

#ifdef GRAPHICS
extern LVAL xmode(V), xcolor(V), xmove(V), xdraw(V),
    xmoverel(V), xdrawrel(V);
extern LVAL xcls(V), xcleol(V), xgotoxy(V);
#endif

#ifdef UNIX
extern LVAL Prim_POPEN(V), Prim_PCLOSE(V);
#endif

#ifdef FILETABLE
extern int getslot _((void));
#endif
#endif /* XLISP_STAT */
