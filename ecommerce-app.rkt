#lang racket

(require web-server/servlet
         web-server/servlet-env)

;; Define a product structure
(struct product (id name price))

;; Example product catalog (list of products)
(define product-catalog
  (list
   (product 1 "Laptop" 1000)
   (product 2 "Phone" 500)
   (product 3 "Tablet" 300)
   (product 4 "Headphones" 150)))

;; Global variable for cart
(define cart '())

;; Find a product by its ID
(define (find-product-by-id catalog id)
  (findf (lambda (p) (= (product-id p) id)) catalog))

;; Add a product to the cart
(define (add-to-cart product-id quantity)
  (define product (find-product-by-id product-catalog product-id))
  (if product
      (set! cart (append cart (list (list product quantity))))
      (begin
        (displayln "Product not found!")
        cart)))

;; Remove a product from the cart
(define (remove-from-cart product-id)
  (set! cart (filter (lambda (item)
                       (not (= (product-id (first item)) product-id)))
                     cart)))

;; Calculate the total price of the cart
(define (calculate-total cart)
  (apply + (map (lambda (item)
                  (* (product-price (first item)) (second item)))
                cart)))

;; Render HTML for the cart
(define (render-cart)
  (define items-html
    (map (lambda (item)
           (define p (first item))
           (define quantity (second item))
           `(li ,(format "~a x~a @ $~a each" (product-name p) quantity (product-price p))))
         cart))
  (define total (calculate-total cart))
  `(div
    (h2 "Your Cart")
    (ul ,@items-html)
    (h3 ,(format "Total: $~a" total))))

;; Serve the webpage
(define (start-web-server)
  (define port (or (getenv "PORT") "8080"))  ; Fetch port from environment variable
  (serve/servlet
   (lambda (req)
     (define query (request-bindings req))

     ;; Use assoc to check if 'add, 'remove, or 'checkout are in the query parameters
     (define add-item (let ([found (assoc 'add query)])
                        (if found (cdr found) #f)))  ;; Extract 'add value or return #f
     (define remove-item (let ([found (assoc 'remove query)])
                           (if found (cdr found) #f)))  ;; Extract 'remove value or return #f
     (define checkout (let ([found (assoc 'checkout query)])
                        (if found (cdr found) #f)))  ;; Extract 'checkout value or return #f

     ;; Add item to the cart if "add" is triggered
     (when add-item
       (add-to-cart (string->number add-item) 1))

     ;; Remove item from the cart if "remove" is triggered
     (when remove-item
       (remove-from-cart (string->number remove-item)))

     ;; Checkout and clear the cart
     (when checkout
       (set! cart '())
       (displayln "Checkout successful!"))

     ;; Render the cart page
     (response/xexpr
      `(html
        (head (title "E-Commerce App"))
        (body
         (h1 "Welcome to the Racket E-Commerce App")
         (form ((method "get"))
           (input ((type "text") (name "add") (placeholder "Product ID")))
           (input ((type "submit") (value "Add to Cart"))))
         (form ((method "get"))
           (input ((type "text") (name "remove") (placeholder "Product ID")))
           (input ((type "submit") (value "Remove from Cart"))))
         ,(render-cart)
         (form ((method "get"))
           (input ((type "hidden") (name "checkout") (value "1")))
           (input ((type "submit") (value "Checkout")))))))))
   #:port (string->number port)))

;; Start the web server
(start-web-server)
