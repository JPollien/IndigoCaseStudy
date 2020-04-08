### Purpose: app that will push API to Heroku for consumption by front end
### Author: Jacob Pollien
### Date: 4/9/2020

from flask import Flask, render_template, url_for
from flask_sqlalchemy import SQLAlchemy
import sys
import json
from flask_heroku import Heroku

app = Flask( __name__ )
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False # disabled to save performance
heroku = Heroku(app)
db = SQLAlchemy(app)


class Dataentry(db.Model):
    __tablename__ = "usda_crops_5yr"

    '''We could use FIPS_CODE as ID, but I could foresee a situation
    wherein the FIPS code changed and messed with our primary key'''
    id = db.Column(db.Integer, primary_key=True)
    CROP = db.Column(db.String())
    FIPS_CODE = db.Column(db.Integer())
    COUNTY_NAME = db.Column(db.String())
    STATE_CODE = db.Column(db.Integer())
    YEAR = db.Column(db.Integer())
    TOTAL_HARVESTED_ACRES = db.Column(db.Integer())
    TOTAL_YIELD = db.Column(db.Numeric())

    def __init__ (self, CROP, FIPS_CODE, COUNTY_NAME, STATE_CODE, YEAR, TOTAL_HARVESTED_ACRES, TOTAL_YIELD):
        self.CROP = crop
        self.FIPS_CODE = FIPS_CODE
        self.COUNTY_NAME = COUNTY_NAME
        self.STATE_CODE = STATE_CODE
        self.YEAR = YEAR
        self.TOTAL_HARVESTED_ACRES = TOTAL_HARVESTED_ACRES
        self.TOTAL_YIELD = TOTAL_YIELD

# Let's set up a route to accept new data so we can keep this updated
@app.route("/submit", methods=["POST"])
def post_to_db():
    indata = Dataentry(request.form['CROP', 'FIPS_CODE', 'COUNTY_NAME', 'STATE_CODE', 'YEAR', 'TOTAL_HARVESTED_ACRES', 'TOTAL_YIELD'])
    data = copy(indata. __dict__ )
    del data["_sa_instance_state"]
    try:
        db.session.add(indata)
        db.session.commit()
    except Exception as e:
        print("\n FAILED entry: {}\n".format(json.dumps(data)))
        print(e)
        sys.stdout.flush()
    return 'Data successfully imported. To enter more data, <a href="{}">click here!</a>'.format(url_for("enter_data"))

@app.route("/")
def enter_data(): 
    return render_template("dataentry.html")

if __name__ == ' __main__':
    #app.debug = True
    app.run()
