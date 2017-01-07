/* run scripts when document is ready */
$(function() {
  "use strict";

  /* add nav class to ul in toc */
  $('#toc ul').each(function() {
    $(this).addClass("nav");
  });

  /* hljs */
  $('pre code').each(function(i, e) {
    hljs.highlightBlock(e);
  });

  /* style tables */
  $('table').addClass('table table-striped table-bordered table-hover table-condensed');

  /* Code block toggles */
  $('.code-chunk button').click(function(e){
      // ensure that we are clicking only on the parent
      $(this).siblings('pre').toggle();
      $(this).parent('.code-chunk').toggleClass('collapsed');
  });

  /* Magnific Popup */
  $(".thumbnail").each(function(){
    $(this).magnificPopup({
        disableOn: 768,
        closeOnContentClick: true,

        type: 'image',
        items: {
          src: $(this).find('img').attr('src'),
          title: $(this).parents('.figure').find('.caption').html()
        }
    });
  });

  /* Rmarkdown's tabbed navigation */
  window.buildTabsets("toc");

  /* Activate tooltips */
  $('[data-toggle="tooltip"]').tooltip({container:'body'});

  /* Activate styled scrollbars for non-webkit browsers */
  var isWebkit = /WebKit/.test(navigator.userAgent) && !/Edge/.test(navigator.userAgent);
  if (!isWebkit) {
    $('.scrollbar').jScrollPane();
  }
});
