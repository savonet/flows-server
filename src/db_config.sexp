(
  ( (Or
      ( (Rule (colnam id))
        (Rule (typnam int))
      )
    )
    ( (serialize "string_of_int")
      (deserialize "int_of_string")
    )
  )
  ( (And
      ( (Or
          ( (Rule (colnam created_at))
            (Rule (typnam timestamptz))
          )
        )
        (Or
          ( (Rule (colnam updated_at))
            (Rule (typnam timestamptz))
          )
        )
      )
    )
    ( (serialize "string_of_float")
      (deserialize "float_of_string")
    )
  )
)
