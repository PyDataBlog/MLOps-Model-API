<link href="<?php echo base_url('html/css/module/leocamera/style.css');?>" rel="stylesheet" type="text/css">
<div class="leocamera_container" >
        <div id="leo-camera" class="camera_wrap" >          
          <div data-thumb="http://demo4leotheme.com/prestashop/leo_metro/cache/cachefs/leocamera/180_90_sample-2.jpg" data-src="http://demo4leotheme.com/prestashop/leo_metro/cache/cachefs/leocamera/940_438_sample-2.jpg" data-link="#">
            <div class="camera_caption fadeFromBottom" >
              <div>                
                <div class="leo_camera_title">
                  <a href="#" title="Sample 2">
                    Sample 2
                  </a>
                </div>
                <div class="leo_camara_desc">
                  Lorem ipsum dolor sit amet consectetur adipiscing quis habitant morbi tristique senectus et netus et malesuada fames 
                </div>
              </div>
            </div>
          </div>
          <div data-thumb="http://demo4leotheme.com/prestashop/leo_metro/cache/cachefs/leocamera/180_90_sample-1.jpg" data-src="http://demo4leotheme.com/prestashop/leo_metro/cache/cachefs/leocamera/940_438_sample-1.jpg" data-link="#">
            <div class="camera_caption fadeFromBottom" >
              <div>                
                <div class="leo_camera_title">
                  <a href="#" title="Sample 2">
                    Sample 2
                  </a>
                </div>
                <div class="leo_camara_desc">
                  Lorem ipsum dolor sit amet consectetur adipiscing quis habitant morbi tristique senectus et netus et malesuada fames 
                </div>
              </div>
            </div>            
          </div>         
        </div>
	  </div>
	   <script>  
        
        $(document).ready(function() {
          
          $('#leo-camera').camera({
            height:'438px',
			alignment			: 'center',
			autoAdvance			: true,
			barDirection			: 'leftToRight',
			barPosition			: 'bottom',
			cols	: 				6,
			easing			: 'easeInOutExpo',
			fx		: 'random',
			hover		: false,
			loader		: 'pie',
			navigation		: false,
			navigationHover		: true,
			pagination		: false,
			playPause		: false,
			pauseOnClick		: false,
			thumbnails		: false,
			time		: 7000,
			transPeriod		: 1500	
          }
                                 );
          
        }
                         );
      </script>