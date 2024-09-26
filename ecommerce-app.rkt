#lang racket

;; Define a product structure
(struct product (id name price))

;; Example product catalog (list of products)
(define product-catalog
  (list
   (product 1 "Laptop" 1000)
   (product 2 "Phone" 500)
   (product 3 "Tablet" 300)
   (product 4 "Headphones" 150)))

;; Find a product by its ID
(define (find-product-by-id catalog id)
  (findf (lambda (p) (= (product-id p) id)) catalog))

;; Add a product to the cart
(define (add-to-cart cart product-id quantity)
  (define product (find-product-by-id product-catalog product-id))
  (if product
      (append cart (list (list product quantity)))
      (begin
        (displayln "Product not found!")
        cart)))

;; Remove a product from the cart
(define (remove-from-cart cart product-id)
  (filter (lambda (item) (not (= (product-id (first item)) product-id))) cart))

;; View the items in the cart
(define (view-cart cart)
  (for-each
   (lambda (item)
     (define p (first item))
     (define quantity (second item))
     (displayln (format "~a x~a @ $~a each" (product-name p) quantity (product-price p))))
   cart))

;; Calculate the total price of the cart
(define (calculate-total cart)
  (apply + (map (lambda (item)
                  (* (product-price (first item)) (second item)))
                cart)))

;; Checkout function
(define (checkout cart)
  (define total (calculate-total cart))
  (displayln (format "Your total is: $~a" total))
  (if (> total 0)
      (begin
        (displayln "Checkout successful!")
        '())
      (displayln "Your cart is empty.")))

;; Define a stack to handle operations
(define stack '())

;; Push an item to the stack
(define (push-stack! arg)
  (set! stack (cons arg stack)))

;; Pop an item from the stack
(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

;; Handle arithmetic operations on the stack
(define (handle [arg #f])
  (cond
    [(number? arg) (push-stack! arg)]
    [(equal? arg '+) 
     (define op-result (+ (pop-stack!) (pop-stack!)))
     (push-stack! op-result)]
    [(equal? arg '*) 
     (define op-result (* (pop-stack!) (pop-stack!)))
     (push-stack! op-result)]
    [else (displayln "Unknown operator.")]))


;; Example usage of the e-commerce app

(define cart '())  ; Start with an empty cart

;; Add items to the cart
(set! cart (add-to-cart cart 1 1)) ; Add 1 Laptop
(set! cart (add-to-cart cart 2 2)) ; Add 2 Phones
(set! cart (add-to-cart cart 4 1)) ; Add 1 Headphones

;; View the cart
(displayln "Your cart contains:")
(view-cart cart)

;; Calculate total
(displayln (format "Total: $~a" (calculate-total cart)))

;; Remove an item from the cart
(set! cart (remove-from-cart cart 2)) ; Remove Phones

;; View the cart after removal
(displayln "\nYour cart after removal:")
(view-cart cart)

;; Checkout
(displayln "\nCheckout:")
(set! cart (checkout cart)) ; Empty the cart after checkout
