var App = function(books){
    var self = {};
    
    self.init = function(books){
        self.renderCreators(books);
        self.renderBooks(books);
    };
    
    self.renderCreators = function(books){
        var creators = [];

        for (var i = 0; i < books.length; i++){
            creators.push(books[i]._creators[0]._name);
        }
        
        creators.sort();
        var occurences = self.count_occurences(creators);

        $.each(occurences, function(creator, n) {
            var $creator = $('.prototype-creator').clone();

            $creator.removeClass('prototype-creator').addClass('creator');

            $creator.find('.name').html(creator + ' (' + n + ')');

            $('#creators').append($creator);
        });
    };
    
    self.renderBooks = function(books){
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

            $('#books').append($book);
        });
    };

    self.count_occurences = function(items){
        var occurences = {};

        $.each(items, function(i, item) {
            if (!occurences[item]){
                occurences[item] = 0;
            }
            
            occurences[item]++;
        });
        
        return occurences;
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