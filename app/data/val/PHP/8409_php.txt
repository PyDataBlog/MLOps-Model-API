<link rel="stylesheet" href="//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css">
<div class="navbar-fixed menu hide">
    <nav>
      <div class="nav-wrapper blue-grey darken-2">
          <a href="#!" class="brand-logo"></a>
        <ul class="right hide-on-med-and-down">
            <?php $dv->genMenu() ?>
        </ul>
      </div>
    </nav>
  </div>

  <div class="navbar-fixed header overLay">
    <nav>
      <div class="nav-wrapper blue-grey">
        <a data-activates="mobile-demo" class="button-collapse"><i class="mdi-navigation-menu"></i></a>
        <a href="/" class="brand-logo center"><img class="logo" /></a>
        <ul class="right hide-on-med-and-down">
            <li class="btn-large waves-effect openMenu blue-grey darken-1"><a onclick="$('.menu').toggleClass('hide'); $('.header').toggleClass('show');" >Menu</a></li>
        </ul>
      </div>
    </nav>
  </div>

<ul class="side-nav" id="mobile-demo">
        <?php $dv->genMenuMobile() ?>
      </ul>

<script>

    $(".button-collapse").sideNav();

</script>