We can use a Higher-Kinded `form` not only for field names and values, but to `validate` form fields

    #EMBED Example.FormValidation data UserForm

Write a validator for each field:

    #EMBED Example.FormValidation validateAge

Then combine them into a record of `Validated` fields

    #EMBED Example.FormValidation validateForm




