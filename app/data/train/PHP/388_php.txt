

    <!-- Jumbotron Header -->
    <header class="jumbotron hero-spacer" style="padding: 2em; text-align: center;">
        <img style="max-width:100%; max-height:100%;" src="<?=base_url()?>assets/img/slogan.png">
        <!--
        <h1>Read and let read!</h1>
        <p>
            Welcome to Discipulus, your humble start up for buying and selling books. We’re dedicated to giving you the very best experience on this website, with a focus on look, feel, functionality, and great community of likeminded people.Welcome to Discipulus, your humble start up for buying and selling books. We’re dedicated to giving you the very best experience on this website, with a focus on look, feel, functionality, and great community of likeminded people.
        </p>
        -->
<!--        <p><a class="btn btn-primary btn-large">Call to action!</a>-->
<!--        </p>-->
    </header>

    <hr>

    <!-- Title -->
<!--    <div class="row">-->
<!--        <div class="col-lg-12">-->
<!--            <h3>Latest Features</h3>-->
<!--        </div>-->
<!--    </div>-->
    <!-- /.row -->

    <!-- Page Features -->
    <div class="row text-center">
        <?php
        if(count($products) == 0){
            echo '<center><i><h3>No data available</h3></i></center>';
        }

        foreach($products as $product){
            ?>
            <div class="col-sm-3 col-lg-3 col-md-3">
                <div class="thumbnail">
                    <div class="thumbnailx">
                        <img src="<?=$product->img?>" style="" alt="">
                    </div>
                    <div class="caption-ownad">
                        <h4><a href="<?=base_url().'shop/viewProduct/'.$product->id?>"><?=$product->title?></a></h4>
                        <span style="font-weight: bold;">P<?=$product->price?>.00</span>
                        <p style="margin-top: 0.3em;"><?=$product->description?></p>
                    </div>
                </div>
            </div>

        <?php
        }
        ?>
    </div>
    <!-- /.row -->
