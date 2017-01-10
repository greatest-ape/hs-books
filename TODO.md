# Important
  - Add a query argument for ignoring the cache
  - Be more granular in catching exceptions from getCoverImage

# Not important
  - Add a last-resort path search for all image files named something like
    cover, if no cover if found by other means. This is probably not necessary
    to implement
  - Cache books, only look again if filenames have changed
