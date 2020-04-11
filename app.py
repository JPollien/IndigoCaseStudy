import os
import os.path
from flask import Flask, render_template
from flask_rest_jsonapi import Api, ResourceDetail, ResourceList, ResourceRelationship
from flask_rest_jsonapi.exceptions import ObjectNotFound
from flask_sqlalchemy import SQLAlchemy
from flask_heroku import Heroku
from sqlalchemy.orm.exc import NoResultFound
from marshmallow_jsonapi.flask import Schema, Relationship
from marshmallow_jsonapi import fields

app = Flask(__name__)
app.config['DEBUG'] = True
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
heroku = Heroku(app)


# Initialize SQLAlchemy
app.config['SQLALCHEMY_DATABASE_URI'] = os.getenv('DATABASE_URL')
db = SQLAlchemy(app)

class DataModel(db.Model):
    __tablename__ = "usda_crops_5yr"

    id = db.Column(db.Integer, primary_key=True)
    CROP = db.Column(db.String())
    FIPS_CODE = db.Column(db.Integer())
    COUNTY_NAME = db.Column(db.String())
    STATE_CODE = db.Column(db.String())
    YEAR = db.Column(db.Integer())
    TOTAL_HARVESTED_ACRES = db.Column(db.Integer())
    TOTAL_YIELD = db.Column(db.Numeric())

class DataSchema(Schema):
    #class Meta:
        '''I include these because I like being able to see where stuff comes
        in case we end up having multiple tables in the same API.
        Plus, metadata can be easily filtered out in the output dict.'''
    #    type_ = 'usda_5yr_crop_table'
    #    self_view = 'data_detail'
    #    self_view_kwargs = {'id': '<id>'}
    #    self_view_many = 'data_list'
        
    id = fields.Int(dump_only=True)
    CROP = fields.Str(required=True)
    FIPS_CODE = fields.Int(required=True)
    COUNTY_NAME = fields.Str(required=True)
    STATE_CODE = fields.Str(required=True)
    YEAR = fields.Int(required=True)
    TOTAL_HARVESTED_ACRES = fields.Int(required=True)
    TOTAL_YIELD = fields.Number(required=True)

# Create resource manager
class DataList(ResourceList):
    schema = DataSchema
    data_layer = {'session': db.session,
                  'model': DataModel}

class DataDetail(ResourceDetail):
    schema = DataSchema
    data_layer = {'session': db.session,
                  'model': DataModel}

api = Api(app)
api.route(DataList, 'data_list', '/data')
api.route(DataDetail, 'data_detail', '/data/<int:id>')

@app.route('/')
def root():
    return render_template('index.html')

if __name__ == '__main__':
    app.run(debug=True)
