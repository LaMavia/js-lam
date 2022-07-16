(
  ( λ one.
    (
      (λ plus.
        (
            (plus one) 
            one
        )
      ) (λ m. 
          (λ n. 
            (λ f. 
              (λ x. 
                ((n f) ((m f) x))
              )
            )
          )
        )
    )
  ) (λ f. (λ x. (f x)))
)
