<?php
if(!empty($_POST['email']))
{
    $data['firstname'] = $_POST['firstname'];
    $data['surname'] = $_POST['surname'];
    $data['email'] = $_POST['email'];
    $data['phone'] = sha1($_POST['phone']);
    $data['ipinterests'] = $_POST['ipinterests'];
    $data['displayvillage'] = $_POST['displayvillage'];
    $data['contactyou'] = $_POST['contactyou'];
    $sendmail = contact_form($data['firstname'],$data['surname'], $data['email'],$data['phone'],$data['ipinterests'],$data['displayvillage'], $data['contactyou']);
    if($sendmail){
        $message = "Send message successful";
    }
    else
        $message = 'Current can not send message. Please try again.';
}
?>
<div class="span4 rightContact">
    <h2>Contact Us</h2>
    <?php
    if($message != "")
    {
        $alert = $logged == true ? "alert-success" : "alert-danger";
        echo '<div class="alert '.$alert.'">'.$message.'</div>';
    }

    ?>
    <form action="" id="contactForm" method="post">
        <p>
            <select class="select" name="sexoption">
                <option value="mr">Mr.</option>
                <option value="mrs">Mrs.</option>
            </select>
        </p>
        <p>
            <input value="" type="text" name="firstname" placeholder="First Name (Required)"/>
        </p>
        <p>
            <input value="" type="text" name="surname" placeholder="Surname"/>
        </p>
        <p>
            <input id="email" value="" type="text" name="email" placeholder="Email Address (Required)"/>
        </p>
        <p>
            <input value="" type="text" name="phone" placeholder="Phone (Required)"/>
        </p>
        <div class="partiBox">
            <h4>Particular Interests <span>(Required)</span></h4>
            <ul>
                <li class="checkboxStyle">
                    <input type="checkbox" id="interest01" name="ipinterests" class="ipinterests">
                    <label for="interest01">
                        Building a new home on vacant land
                    </label>
                </li>
                <li class="checkboxStyle">
                    <input type="checkbox" id="interest02" name="ipinterests" class="ipinterests">
                    <label for="interest02">
                        Knockdown Rebuild (KDR)
                    </label>
                </li>
                <li class="checkboxStyle">
                    <input type="checkbox" id="interest03" name="ipinterests" class="ipinterests">
                    <label for="interest03">
                        Home & Land Package
                    </label>
                </li>
                <li class="checkboxStyle">
                    <input type="checkbox" id="interest04" name="ipinterests" class="ipinterests">
                    <label for="interest04">
                        Dual Occupancy or Duplex
                    </label>
                </li>
                <li class="checkboxStyle">
                    <input type="checkbox" id="interest05" name="ipinterests" class="ipinterests">
                    <label for="interest05">
                        Building for Investment
                    </label>
                </li>
                <li class="checkboxStyle">
                    <input type="checkbox" id="interest06" name="ipinterests" class="ipinterests">
                    <label for="interest06">
                        Multi Residential
                    </label>
                </li>
            </ul>
            <div class="selectGroup">
                <select class="select" name="displayvillage">
                    <option value="0">Closest Display Village?</option>
                    <option value="display-center-locations">Display Centre Locations</option>
                    <option value="display-home-for-sale">Display Homes for Sale</option>
                    <option value="warwick-farm-display-village">Warwick Farm Display Village</option>
                </select>
                <select class="select" name="contactyou">
                    <option value="0">How should we contact you?</option>
                    <option value="phone">Phone</option>
                    <option value="email">Email</option>
                </select>
            </div>
        </div>
        <input type="submit" type="SUBMIT" value="SEND"/>
    </form>
</div>
<script type="text/javascript">
    $(document).ready(function() {
        jQuery.validator.addMethod('selectcheck', function (value) {
            return (value != '0');
        }, "");

        $("#contactForm").validate({
            rules: {
                'firstname': {
                    required: true
                },
                'surname': {
                    required: true
                },
                'phone': {
                    required: true
                },
                'email': {
                    required: true,
                    email: true
                },
                'ipinterests': {
                    required: true
                },
                'displayvillage': {
                    selectcheck: true
                },
                'contactyou': {
                    selectcheck: true
                }
            },

            errorPlacement: function(error, element){},
            highlight: function(element) {
                //console.log(element);
                if($(element).is(':checkbox'))
                {
                    var name = $(element).attr('name');
                    $('input[name='+name+']').parent('.checkboxStyle').addClass('error');
                }
                else
                {
                    $(element).addClass('error');
                }
            },
            unhighlight: function(element, errorClass, validClass) {
                $(element).removeClass(errorClass).addClass(validClass); // remove error class from elements/add valid class
                $('.checkboxStyle').removeClass(errorClass); // remove error class from ul element for checkbox groups and radio inputs
            },
            submitHandler: function(form) {
                var boxes = $('.ipinterests:checkbox');
                if(boxes.length > 0) {
                    if( $('.ipinterests:checkbox:checked').length < 1) {
                        alert('Please select at least one checkbox');
                        boxes[0].focus();
                        return false;
                    }
                }
                form.submit();
            },
        });
    });
</script>