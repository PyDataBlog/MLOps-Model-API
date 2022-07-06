// autocomplet : this function will be executed every time we change the text
function autocomplet() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code0').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id').show();
				$('#itemcode_list_id').html(data);
			}
		});
	} else {
		$('#itemcode_list_id').hide();
	}
}

//set_item : this function will be executed when we select an item
function set_item(item) {
	// change input value
	$('#code0').val(item);
	// hide proposition list
	$('#itemcode_list_id').hide();
}

//=================================================== This is for code1 ============================================
function autocomplet_1() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code1').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code1.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_1').show();
				$('#itemcode_list_id_1').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_1').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_1(item) {
	// change input value
	$('#code1').val(item);
	// hide proposition list
	$('#itemcode_list_id_1').hide();
}
//================================================== End of code1 ===================================================

//=================================================== This is for code2 ============================================
function autocomplet_2() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code2').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code2.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_2').show();
				$('#itemcode_list_id_2').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_2').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_2(item) {
	// change input value
	$('#code2').val(item);
	// hide proposition list
	$('#itemcode_list_id_2').hide();
}
//================================================== End of code2 ===================================================

//=================================================== This is for code3 ============================================
function autocomplet_3() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code3').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code3.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_3').show();
				$('#itemcode_list_id_3').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_3').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_3(item) {
	// change input value
	$('#code3').val(item);
	// hide proposition list
	$('#itemcode_list_id_3').hide();
}
//================================================== End of code3 ===================================================

//=================================================== This is for code4 ============================================
function autocomplet_4() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code4').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code4.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_4').show();
				$('#itemcode_list_id_4').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_4').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_4(item) {
	// change input value
	$('#code4').val(item);
	// hide proposition list
	$('#itemcode_list_id_4').hide();
}
//================================================== End of code4 ===================================================

//=================================================== This is for code5 ============================================
function autocomplet_5() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code5').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code5.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_5').show();
				$('#itemcode_list_id_5').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_5').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_5(item) {
	// change input value
	$('#code5').val(item);
	// hide proposition list
	$('#itemcode_list_id_5').hide();
}
//================================================== End of code5 ===================================================

//=================================================== This is for code6 ============================================
function autocomplet_6() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code6').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code6.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_6').show();
				$('#itemcode_list_id_6').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_6').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_6(item) {
	// change input value
	$('#code6').val(item);
	// hide proposition list
	$('#itemcode_list_id_6').hide();
}
//================================================== End of code6 ===================================================

//=================================================== This is for code7 ============================================
function autocomplet_7() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code7').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code7.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_7').show();
				$('#itemcode_list_id_7').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_7').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_7(item) {
	// change input value
	$('#code7').val(item);
	// hide proposition list
	$('#itemcode_list_id_7').hide();
}
//================================================== End of code7 ===================================================

//=================================================== This is for code8 ============================================
function autocomplet_8() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code8').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code8.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_8').show();
				$('#itemcode_list_id_8').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_8').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_8(item) {
	// change input value
	$('#code8').val(item);
	// hide proposition list
	$('#itemcode_list_id_8').hide();
}
//================================================== End of code8 ===================================================

//=================================================== This is for code9 ============================================
function autocomplet_9() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code9').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code9.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_9').show();
				$('#itemcode_list_id_9').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_9').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_9(item) {
	// change input value
	$('#code9').val(item);
	// hide proposition list
	$('#itemcode_list_id_9').hide();
}
//================================================== End of code9 ===================================================

//=================================================== This is for code10 ============================================
function autocomplet_10() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code10').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code10.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_10').show();
				$('#itemcode_list_id_10').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_10').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_10(item) {
	// change input value
	$('#code10').val(item);
	// hide proposition list
	$('#itemcode_list_id_10').hide();
}
//================================================== End of code10 ===================================================

//=================================================== This is for code11 ============================================
function autocomplet_11() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code11').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code11.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_11').show();
				$('#itemcode_list_id_11').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_11').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_11(item) {
	// change input value
	$('#code11').val(item);
	// hide proposition list
	$('#itemcode_list_id_11').hide();
}
//================================================== End of code11 ===================================================

//=================================================== This is for code12 ============================================
function autocomplet_12() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code12').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code12.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_12').show();
				$('#itemcode_list_id_12').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_12').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_12(item) {
	// change input value
	$('#code12').val(item);
	// hide proposition list
	$('#itemcode_list_id_12').hide();
}
//================================================== End of code12 ===================================================

//=================================================== This is for code13 ============================================
function autocomplet_13() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code13').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code13.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_13').show();
				$('#itemcode_list_id_13').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_13').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_13(item) {
	// change input value
	$('#code13').val(item);
	// hide proposition list
	$('#itemcode_list_id_13').hide();
}
//================================================== End of code13 ===================================================

//=================================================== This is for code14 ============================================
function autocomplet_14() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code14').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code14.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_14').show();
				$('#itemcode_list_id_14').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_14').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_14(item) {
	// change input value
	$('#code14').val(item);
	// hide proposition list
	$('#itemcode_list_id_14').hide();
}
//================================================== End of code14 ===================================================

//=================================================== This is for code15 ============================================
function autocomplet_15() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code15').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code15.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_15').show();
				$('#itemcode_list_id_15').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_15').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_15(item) {
	// change input value
	$('#code15').val(item);
	// hide proposition list
	$('#itemcode_list_id_15').hide();
}
//================================================== End of code15 ===================================================

//=================================================== This is for code16 ============================================
function autocomplet_16() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code16').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code16.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_16').show();
				$('#itemcode_list_id_16').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_16').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_16(item) {
	// change input value
	$('#code16').val(item);
	// hide proposition list
	$('#itemcode_list_id_16').hide();
}
//================================================== End of code16 ===================================================

//=================================================== This is for code17 ============================================
function autocomplet_17() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code17').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code17.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_17').show();
				$('#itemcode_list_id_17').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_17').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_17(item) {
	// change input value
	$('#code17').val(item);
	// hide proposition list
	$('#itemcode_list_id_17').hide();
}
//================================================== End of code17 ===================================================

//=================================================== This is for code18 ============================================
function autocomplet_18() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code18').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code18.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_18').show();
				$('#itemcode_list_id_18').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_18').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_18(item) {
	// change input value
	$('#code18').val(item);
	// hide proposition list
	$('#itemcode_list_id_18').hide();
}
//================================================== End of code18 ===================================================

//=================================================== This is for code19 ============================================
function autocomplet_19() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code19').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code19.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_19').show();
				$('#itemcode_list_id_19').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_19').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_19(item) {
	// change input value
	$('#code19').val(item);
	// hide proposition list
	$('#itemcode_list_id_19').hide();
}
//================================================== End of code19 ===================================================

//=================================================== This is for code20 ============================================
function autocomplet_20() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#code20').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_code20.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemcode_list_id_20').show();
				$('#itemcode_list_id_20').html(data);
			}
		});
	} else {
		$('#itemcode_list_id_20').hide();
	}
}

// set_item : this function will be executed when we select an item
function set_item_20(item) {
	// change input value
	$('#code20').val(item);
	// hide proposition list
	$('#itemcode_list_id_20').hide();
}
//================================================== End of code20 ===================================================


//================================================== Satrt of desc0 ======================================================
function autocomplet2() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc0').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id').show();
				$('#itemdesc_list_id').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id').hide();
	}
}

function set_item2(item) {
	// change input value
	$('#desc0').val(item);
	// hide proposition list
	$('#itemdesc_list_id').hide();
}
//================================================== End of desc0 ======================================================

//================================================== Satrt of desc1 ======================================================
function autocomplet2_1() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc1').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_1.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_1').show();
				$('#itemdesc_list_id_1').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_1').hide();
	}
}

function set_item2_1(item) {
	// change input value
	$('#desc1').val(item);
	// hide proposition list
	$('#itemdesc_list_id_1').hide();
}
//================================================== End of desc1 ======================================================

//================================================== Satrt of desc2 ======================================================
function autocomplet2_2() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc2').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_2.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_2').show();
				$('#itemdesc_list_id_2').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_2').hide();
	}
}

function set_item2_2(item) {
	// change input value
	$('#desc2').val(item);
	// hide proposition list
	$('#itemdesc_list_id_2').hide();
}
//================================================== End of desc2 ======================================================

//================================================== Satrt of desc3 ======================================================
function autocomplet2_3() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc3').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_3.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_3').show();
				$('#itemdesc_list_id_3').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_3').hide();
	}
}

function set_item2_3(item) {
	// change input value
	$('#desc3').val(item);
	// hide proposition list
	$('#itemdesc_list_id_3').hide();
}
//================================================== End of desc3 ======================================================

//================================================== Satrt of desc4 ======================================================
function autocomplet2_4() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc4').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_4.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_4').show();
				$('#itemdesc_list_id_4').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_4').hide();
	}
}

function set_item2_4(item) {
	// change input value
	$('#desc4').val(item);
	// hide proposition list
	$('#itemdesc_list_id_4').hide();
}
//================================================== End of desc4 ======================================================

//================================================== Satrt of desc5 ======================================================
function autocomplet2_5() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc5').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_5.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_5').show();
				$('#itemdesc_list_id_5').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_5').hide();
	}
}

function set_item2_5(item) {
	// change input value
	$('#desc5').val(item);
	// hide proposition list
	$('#itemdesc_list_id_5').hide();
}
//================================================== End of desc5 ======================================================

//================================================== Satrt of desc6 ======================================================
function autocomplet2_6() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc6').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_6.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_6').show();
				$('#itemdesc_list_id_6').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_6').hide();
	}
}

function set_item2_6(item) {
	// change input value
	$('#desc6').val(item);
	// hide proposition list
	$('#itemdesc_list_id_6').hide();
}
//================================================== End of desc6 ======================================================

//================================================== Satrt of desc7 ======================================================
function autocomplet2_7() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc7').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_7.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_7').show();
				$('#itemdesc_list_id_7').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_7').hide();
	}
}

function set_item2_7(item) {
	// change input value
	$('#desc7').val(item);
	// hide proposition list
	$('#itemdesc_list_id_7').hide();
}
//================================================== End of desc7 ======================================================

//================================================== Satrt of desc8 ======================================================
function autocomplet2_8() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc8').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_8.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_8').show();
				$('#itemdesc_list_id_8').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_8').hide();
	}
}

function set_item2_8(item) {
	// change input value
	$('#desc8').val(item);
	// hide proposition list
	$('#itemdesc_list_id_8').hide();
}
//================================================== End of desc8 ======================================================

//================================================== Satrt of desc9 ======================================================
function autocomplet2_9() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc9').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_9.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_9').show();
				$('#itemdesc_list_id_9').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_9').hide();
	}
}

function set_item2_9(item) {
	// change input value
	$('#desc9').val(item);
	// hide proposition list
	$('#itemdesc_list_id_9').hide();
}
//================================================== End of desc9 ======================================================

//================================================== Satrt of desc10 ======================================================
function autocomplet2_10() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc10').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_10.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_10').show();
				$('#itemdesc_list_id_10').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_10').hide();
	}
}

function set_item2_10(item) {
	// change input value
	$('#desc10').val(item);
	// hide proposition list
	$('#itemdesc_list_id_10').hide();
}
//================================================== End of desc10 ======================================================

//================================================== Satrt of desc11 ======================================================
function autocomplet2_11() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc11').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_11.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_11').show();
				$('#itemdesc_list_id_11').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_11').hide();
	}
}

function set_item2_11(item) {
	// change input value
	$('#desc11').val(item);
	// hide proposition list
	$('#itemdesc_list_id_11').hide();
}
//================================================== End of desc11 ======================================================

//================================================== Satrt of desc12 ======================================================
function autocomplet2_12() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc12').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_12.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_12').show();
				$('#itemdesc_list_id_12').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_12').hide();
	}
}

function set_item2_12(item) {
	// change input value
	$('#desc12').val(item);
	// hide proposition list
	$('#itemdesc_list_id_12').hide();
}
//================================================== End of desc12 ======================================================

//================================================== Satrt of desc13 ======================================================
function autocomplet2_13() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc13').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_13.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_13').show();
				$('#itemdesc_list_id_13').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_13').hide();
	}
}

function set_item2_13(item) {
	// change input value
	$('#desc13').val(item);
	// hide proposition list
	$('#itemdesc_list_id_13').hide();
}
//================================================== End of desc13 ======================================================

//================================================== Satrt of desc14 ======================================================
function autocomplet2_14() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc14').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_14.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_14').show();
				$('#itemdesc_list_id_14').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_14').hide();
	}
}

function set_item2_14(item) {
	// change input value
	$('#desc14').val(item);
	// hide proposition list
	$('#itemdesc_list_id_14').hide();
}
//================================================== End of desc14 ======================================================

//================================================== Satrt of desc15 ======================================================
function autocomplet2_15() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc15').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_15.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_15').show();
				$('#itemdesc_list_id_15').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_15').hide();
	}
}

function set_item2_15(item) {
	// change input value
	$('#desc15').val(item);
	// hide proposition list
	$('#itemdesc_list_id_15').hide();
}
//================================================== End of desc15 ======================================================

//================================================== Satrt of desc16 ======================================================
function autocomplet2_16() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc16').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_16.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_16').show();
				$('#itemdesc_list_id_16').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_16').hide();
	}
}

function set_item2_16(item) {
	// change input value
	$('#desc16').val(item);
	// hide proposition list
	$('#itemdesc_list_id_16').hide();
}
//================================================== End of desc16 ======================================================

//================================================== Satrt of desc17 ======================================================
function autocomplet2_17() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc17').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_17.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_17').show();
				$('#itemdesc_list_id_17').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_17').hide();
	}
}

function set_item2_17(item) {
	// change input value
	$('#desc17').val(item);
	// hide proposition list
	$('#itemdesc_list_id_17').hide();
}
//================================================== End of desc17 ======================================================

//================================================== Satrt of desc18 ======================================================
function autocomplet2_18() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc18').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_18.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_18').show();
				$('#itemdesc_list_id_18').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_18').hide();
	}
}

function set_item2_18(item) {
	// change input value
	$('#desc18').val(item);
	// hide proposition list
	$('#itemdesc_list_id_18').hide();
}
//================================================== End of desc18 ======================================================

//================================================== Satrt of desc19 ======================================================
function autocomplet2_19() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc19').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_19.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_19').show();
				$('#itemdesc_list_id_19').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_19').hide();
	}
}

function set_item2_19(item) {
	// change input value
	$('#desc19').val(item);
	// hide proposition list
	$('#itemdesc_list_id_19').hide();
}
//================================================== End of desc19 ======================================================

//================================================== Satrt of desc120 ======================================================
function autocomplet2_20() {
	var min_length = 0; // min caracters to display the autocomplete
	var keyword = $('#desc20').val();
	if (keyword.length >= min_length) {
		$.ajax({
			url: 'ajax_refresh_item_desc_20.php',
			type: 'POST',
			data: {keyword:keyword},
			success:function(data){
				$('#itemdesc_list_id_20').show();
				$('#itemdesc_list_id_20').html(data);
			}
		});
	} else {
		$('#itemdesc_list_id_20').hide();
	}
}

function set_item2_20(item) {
	// change input value
	$('#desc20').val(item);
	// hide proposition list
	$('#itemdesc_list_id_20').hide();
}
//================================================== End of desc20 ======================================================

