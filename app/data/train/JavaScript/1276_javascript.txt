document.addEventListener("DOMContentLoaded", function(event) {
	if (/android|blackberry|iPhone|iPad|iPod|webOS/i.test(navigator.userAgent) === false) {
		var linkDescrs = document.querySelectorAll('.link-descr');
		Array.prototype.forEach.call(linkDescrs, function(el, i) {
			el.parentNode.addEventListener('mouseenter', animateTypeText);
			el.parentNode.addEventListener('mouseleave', destroyTypeText);
		});
	} else {
		var linkDescrs = document.querySelectorAll('.link-descr');
		Array.prototype.forEach.call(linkDescrs, function(el, i) {
			removeClass(el, 'link-descr');
		});
	}
});

var addClass = function(elem, add_class) {
	elem.setAttribute("class", elem.getAttribute("class") + ' ' + add_class);
};
var removeClass = function(elem, rem_class) {
	var origClass = elem.getAttribute("class");
	var remClassRegex = new Regexp("(\\s*)"+rem_class+"(\\s*)");
	var classMatch = origClass.match(remClassRegex);
	var replaceString = '';
	if (classMatch[1].length > 0 || classMatch[2].length > 0) {
		replaceString = ' ';
	}
	var newClass = origClass.replace(remClassRegex, replaceString);
	elem.setAttribute("class", newClass);
};
var animateTypeText = function() {
	var elem = this;
	var typeArea = document.createElement("span");
	typeArea.setAttribute("class", "link-subtext");
	elem.insertBefore(typeArea, elem.querySelector("span:last-of-type"));
	setTimeout(addLetter(elem), 40);
};
var addLetter = function(elem) {
//	if (elem.parentElement.querySelector(":hover") === elem) {
		var subtextSpan = elem.querySelector(".link-subtext");
		var descrText = elem.querySelector(".link-descr").textContent;
		if (subtextSpan === null) {
			return;
		}
		var currentText = subtextSpan.textContent.slice(0,-1);
		var currentPos = currentText.length;
		subtextSpan.textContent = currentText +
			descrText.slice(currentPos, currentPos+1) + "\u258B";
		if (currentText.length < descrText.length) {
			setTimeout(function(){addLetter(elem)}, 40);
		}
//	}
};
var destroyTypeText = function() {
	var elem = this;
	elem.removeChild(elem.querySelector('.link-subtext'));
};
