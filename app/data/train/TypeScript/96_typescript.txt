module ngFoundation.directives {

	//@NgDirective('topBarSection')
    class TopBarSectionDirective implements ng.IDirective {
        template = '<section class="top-bar-section" ng-transclude></section>';
		restrict = "E";
		transclude = true;
		replace = true;
	}
} 