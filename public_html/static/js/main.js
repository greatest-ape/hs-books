var App = function(books){
    var self = {};
    
    self.init = function(books){
        $.each(books, function(i, book){
            var $book = $('.prototype-book').clone();
            
            $book.removeClass('prototype-book').addClass('book');

            var cover = book._maybeCover;
            
            if (cover){
                var s = 'data:' + cover._mediaType + ';base64,' + cover._image;
                
                $book.find('.image a').attr('href', book._path);
                $book.find('img').attr('src', s);
            }
            
            $book.find('.title').html(book._titles[0]);
            $book.find('.creator').html(book._creators[0]._name);
            
            $('body').append($book);
        });
    };
    
    self.init(books);
    
    return self;
};

(function($){
    $(function(){
        $.getJSON('app.cgi', function(books){
            App(books);
        });
    });
})(jQuery);