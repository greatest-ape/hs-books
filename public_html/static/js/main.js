var Book = function(title, path, cover, author, $author){
    var self = {};

    self.title   = title;
    self.path    = path;
    self.cover   = cover;
    self.author  = author;
    self.$author = $author;

    self.$bookWithImage = null;
    self.$bookInList = null;

    self.init = function(){
        self.renderWithImage();
        self.renderInList();
    }

    self.matchesKeywords = function(keywords){
        return keywords.all(function(keyword){
            return self.title.includes(keyword)
        })
    }
    
    self.show = function(){
        self.$bookWithImage.show();
        self.$bookInList.show();
    }
    
    self.hide = function(){
        self.$bookWithImage.hide();
        self.$bookInList.hide();
    }

    self.renderWithImage = function(){
        var $book = $('.prototype-book').clone();

        $book.removeClass('prototype-book').addClass('book');

        if (self.cover){
            $book.find('.image a').attr('href', self.path);
            $book.find('img').attr('src', self.cover._thumbnailPath);
        }

        $book.find('.title').html(self.title);
        $book.find('.creator').html(self.author);

        $('#books').append($book);

        self.$bookWithImage = $book;
    }

    self.renderInList = function(){
        var $book = $('.prototype-creator-book').clone();
        
        $book.removeClass('prototype-creator-book').addClass('book');
        $book.find('a').attr('href', self.path).html(self.title);
        
        self.$author.find('.books').append($book);

        self.$bookInList = $book;
    }

    self.init();

    return self;
}


var Author = function(name, books){
    var self = {};

    self.name = "";
    self.books = [];

    self.$author = null;

    self.init = function(){
        self.name = name;

        self.render();

        self.books = books.map(function(book){
            return Book(book._titles[0], book._path, book._maybeCover, self.name, self.$author);
        });
    }

    self.showOnMatch = function(keywords){
        var matchingBooks = self.books.filter(function(book){
            return book.matchesKeywords(keywords);
        })

        var authorMatch = keywords.all(function(keyword){
            return self.title.includes(keyword)
        })

        // Display all books from this author
        if (authorMatch) {
            self.$author.show();

            $.each(self.books, function(i, book) { book.show() });
        }
        // Display all matching books
        else if (matchingBooks.length > 0) {
            self.$author.show();

            $.each(matchingBooks, function(i, book) { book.show() });
        }
        // Hide author and books
        else {
            self.$author.hide();

            $.each(self.books, function(i, book) { book.hide() });
        }
    }

    self.render = function(){
        self.$author = $('.prototype-creator').clone();

        self.$author.removeClass('prototype-creator').addClass('creator');

        self.$author.find('.name').html(self.name);

        self.$author.appendTo($('#creators'));
    }

    self.init();

    return self;
}


var App = function($, books){
    var self = {};
    
    self.authors = [];

    self.init = function(books){
        var creators = self.groupBooksByAuthor(books);
        
        $('#loading').hide();

        self.createAuthors(creators);

        self.makeAuthorsGoToBottom();
    };
    
    self.groupBooksByAuthor = function(books){
        var authors = {};

        // Add books to their author
        
        $.each(books, function(i, book){
            var author = book._creators[0];

            if (!(author in authors)){
                authors[author] = [];
            }

            authors[author].push(book);
        });
        
        // Sort books within the authors
        
        Object.keys(authors).map(function(key, index){
            authors[key].sort(function(book1, book2) {
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
        });
        
        return authors;
    };

    self.createAuthors = function(creators){
        self.iterate_over_dict_sorted(creators, function(creator, books) {
            self.authors.push(Author(creator, books));
        });
    }
    
    self.iterate_over_dict_sorted = function(dict, f){
        var keys = Object.keys(dict);
        keys.sort();
        
        $.each(keys, function(i, key){
            f(key, dict[key]);
        });
    };
    
    self.makeAuthorsGoToBottom = function(){
        if ($(window).width() > 650){
            $('#creators').height($(document).height());
        }
    };
    
    self.init(books);
    
    return self;
};

(function($){
    $(function(){
        $.getJSON('app.cgi', function(books){
            App($, books);
        });
    });
})(jQuery);
