@extends('layout.layout-custome')

@section('title')
 Danh sách sản phẩm 
@endsection

@section('content')
<?php $url = URL::to('/')."/images"; ?>
<!--BACKTOP: END-->
            <div class="clear"></div>
            <!--Hộp chứa nội dung sản phẩm:START--><!-- InstanceBeginEditable name="container" -->
            <div id="container">
            	<div id="column-right">
                 </div>
                <div id="content">
                	<div class="fast-link">
                            <a href="{{Asset('home')}}">Trang chủ</a>
                                » 
                            <a href=""> Danh sách sản phẩm</a>
                    </div>
                    <h1>Danh sách sản phẩm</h1>
                    <div class="product-list">
                        <ul>
                            <?php $i = 0;
                            $url = URL::to('/')."/images";
                            while($i<count($products)){ ?>
                            <li class="first-in-line">
                           
                                <div class="right">
                                    <div class="price">
                                        <span class="price-new">
                                           <?php echo $products[$i]->price; ?>
                                        </span>
                                        <span class="price-old">
                                            $200.00
                                        </span>
                                        <span class="price-tax">
                                            Ex Tax: $199.00
                                        </span>
                                    </div>
                                    <div class="cart">
                                        <form method="get" action="{{Asset('giohang')}}" class="inline"  id="formSP">
                                        <button class = "button-prod" style="background:#D45F09" value = "<?php echo $products[$i]->id; ?>">
                                            <span>Vào giỏ</span>
                                        </button>
                                      </form>
                                    </div>
                                </div>
                                <div class="left">
                                    <div class="image">
                                            <img width = "500px;" height="400px;" id="img_45" alt="Dolor sit amet" title="<?php echo $products[$i]->image; ?>" src="<?php echo $url.$products[$i]->image;?>"></img>
                                    </div>
                                    <div class="name">
                                        <a>
                                            <?php echo $products[$i]->name; ?>
                                        </a>
                                    </div>
                                    <div class="description">
                                        <?php echo $products[$i]->describe; ?>
                                    </div>
                                    <div class="rating">
                                    </div>
                                </div>
                            
                            </li>
                        </form>
                            <?php $i++; }?>
                           
                        </ul>
                    </div>

                            </div>
                        </div>
            
                    </div>
                   
                    
                    
                </div> 
                <script type="text/javascript">
                	$(document).ready(function() {
                 $('#formSP').submit(function(){
        var id= $('#id').val();
        $.ajax({
           type: 'post',
           url: '{{Asset('getUserSp')}}',
           data: 
           {
            id: id,
           },
           dataType: 'json',
           success:function(data){
             if(data==0){
               alert("Bạn cần phải đăng nhập khi mua sản phẩm :!");
             }
             else{
               document.forms["formSP"].submit()
            }
           }
        });
     return false;
    });  
    }); 
function display(view) {
	if (view == 'list') {
		$('.product-grid ').attr('class', 'product-list');
		
		$('.product-list ul li').each(function(index, element) {
			html  = '<div class="right">';
				html += '  <div class="price">' + $(element).find('.price').html() + '</div>';
			html += '  <div class="cart">' + $(element).find('.cart').html() + '</div>';
			html += '  <div class="wishlist">' + $(element).find('.wishlist').html() + '</div>';
			html += '  <div class="compare">' + $(element).find('.compare').html() + '</div>';
		
			html += '</div>';			
			
			html += '<div class="left">';
			
			var image = $(element).find('.image').html();
			
			if (image != null) { 
				html += '<div class="image">' + image + '</div>';
			}
			
	
					
			html += '  <div class="name">' + $(element).find('.name').html() + '</div>';
			html += '  <div class="description">' + $(element).find('.description').html() + '</div>';
			
			var rating = $(element).find('.rating').html();
			
			if (rating != null) {
				html += '<div class="rating">' + rating + '</div>';
			}
				
			html += '</div>';

						
			$(element).html(html);
		});		
		
		$('.display').html('<b>Display:</b> <div id="list_b"></div> <a id="grid_a" onclick="display(\'grid\');">Grid</a>');
		
		$.cookie('display', 'list'); 
	} else {
		$('.product-list').attr('class', 'product-grid');
		
		$('.product-grid ul li').each(function(index, element) {
			html = '';
					
			var image = $(element).find('.image').html();
			
			if (image != null) {
				html += '<div class="image">' + image + '</div>';
			}
			
		html += '<div class="name">' + $(element).find('.name').html() + '</div>';
			
			
			var price = $(element).find('.price').html();
			
			if (price != null) {
				html += '<div class="price">' + price  + '</div>';
			}
	
			html += '<div class="description">' + $(element).find('.description').html() + '</div>';
			
			var rating = $(element).find('.rating').html();
			
			if (rating != null) {
				html += '<div class="rating">' + rating + '</div>';
			}
						
			html += '<div class="cart">' + $(element).find('.cart').html() + '</div>';
			html += '<div class="wishlist">' + $(element).find('.wishlist').html() + '</div>';
			html += '<div class="compare">' + $(element).find('.compare').html() + '</div>';
			
			$(element).html(html);
		});	
					
		$('.display').html('<b>Display:</b> <a id="list_a" onclick="display(\'list\');">List</a>  <div id="grid_b"></div>');
		
		$.cookie('display', 'grid');
	}
}

view = $.cookie('display');

if (view) {
	display(view);
} else {
	display('list');
}
</script>   
           
@endsection
