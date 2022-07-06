subroutine dialog_res(dlgbox,res_x,res_y)
	use dflib
	use dialogm
	include 'resources.fd'
	type (dialog) dlgbox
	logical::test,pushed_state
	integer(4)::res_x,res_y
	test=dlginit(IDD_resolution,dlgbox)
	if (test==.false.) then
		print*,'failed to initialize dialog box'
		return
	end if
	test=dlgmodal(dlgbox)
	test=dlgget(dlgbox,IDC_res_640,pushed_state)
	if (pushed_state) then
		res_x=640
		res_y=480
	end if
	test=dlgget(dlgbox,IDC_res_800,pushed_state)
	if (pushed_state) then
		res_x=800
		res_y=600
	end if
		test=dlgget(dlgbox,IDC_res_1024,pushed_state)
	if (pushed_state) then
		res_x=1024
		res_y=768
	end if
		test=dlgget(dlgbox,IDC_res_1280,pushed_state)
	if (pushed_state) then
		res_x=1280
		res_y=1024
	end if
end subroutine dialog_res
	