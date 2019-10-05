$(document).ready(function () {
        var url = window.location;
        $('.navbar-top').find('.active').removeClass('active');
        $('.navbar-top li a').each(function () {
            if (this.href == url) {
                $(this).parent().addClass('active');
            }
        });
   });
