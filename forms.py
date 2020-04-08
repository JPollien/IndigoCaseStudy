from wtforms import Form, StringField, SelectField

class CropSearchForm(Form):
    choices = [('CROPS', 'CROPS'),
               ('COUNTY_NAME', 'COUNTY_NAME'),
               ('STATE_CODE', 'STATE_CODE'),
               ('YEAR', 'YEAR')]
    select = SelectField('Search for yields:', choices=choices)
    search = StringField('')
