We can render and parse `form`s via a record. This can be a simple record:

    #EMBED Example.FormSimple data ContactForm

Using a simple record requires you to match the field names manually: 

    #EMBED Example.FormSimple nameForm

Alternatively, use a Higher-Kinded record, and use `fieldNames` to have the compiler help:

    #EMBED Example.FormSimple data ContactForm'

    #EMBED Example.FormSimple nameForm'

