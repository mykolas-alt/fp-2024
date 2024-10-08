```diff
 
-string = character | character string
+string = char | char string
 
-title = string
+title = alphanumeric+
 
+root = "init " rental_store | "addMovies " movie_list | "addMovie " movie | "takeMovie " movie | "removeMovie " movie | "show"
 rental_store = "VHS Rental Store" ':' (catalog | customer_database | rental_transactions)
 
```