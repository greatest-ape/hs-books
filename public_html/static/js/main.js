var App = function(books){
    var self = {};

    self.init = function(books){
        var creators = self.buildCreators(books);
        
        self.renderCreators(creators);
        self.renderBooks(creators);
    };
    
    self.buildCreators = function(books){
        
        var creators = {};

        // Add books to their creator
        
        $.each(books, function(i, book){
            var creator = books[i]._creators[0]._name;

            if (!creators[creator]){
                creators[creator] = [];
            }

            creators[creator].push(book);
        });
        
        // Sort books within the creators
        
        var creators_sorted = creators;

        $.each(creators, function(creator, books){
            books.sort(function(book1, book2) {
                var title1 = book1._titles[0];
                var title2 = book2._titles[0];

                if (title1 > title2) {
                    return 1;
                }
                else if (title1 < title2) {
                    return -1;
                }
                else {
                    return 0;
                }
            });
            
            creators_sorted[creator] = books;
        });
        
        return creators_sorted;
    };
    
    self.renderCreators = function(creators){
        var creator_list = Object.keys(creators);
        creator_list.sort();

        $.each(creator_list, function(i, creator) {
            var books = creators[creator];
            
            var $creator = $('.prototype-creator').clone();

            $creator.removeClass('prototype-creator').addClass('creator');

            $creator.find('.name').html(creator + ' (' + books.length + ')');
            
            $.each(books, function(i, book){
                var $book = $('.prototype-creator-book').clone();
                
                $book.removeClass('prototype-creator-book').addClass('book');
                $book.html(book._titles[0]);
                
                $creator.find('.books').append($book)
            });

            $('#creators').append($creator);
        });
    };
    
    self.renderBooks = function(creators){
        var creator_list = Object.keys(creators);
        creator_list.sort();
        
        $.each(creator_list, function(i, creator){
            var books = creators[creator];
            
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