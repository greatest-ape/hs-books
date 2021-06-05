#!/bin/bash

chgrp -R www-data html
chmod -R g+rw html/media/covers

# To enable writing .cache file
chmod g+rw html
