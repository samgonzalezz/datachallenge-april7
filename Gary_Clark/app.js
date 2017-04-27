`use strict`

const fs = require(`fs`);
const express = require(`express`);
const app = express();
const port = process.env.port || 8000;

////////////// Get the University data from the CollegeScoreCard API available at https://api.data.gov/docs/ed/ /////////////////////////////
let univ;
////////////// Use this section when hitting the API with registered key ////////////////////////
// const univ = require(`./uniData`)
// univ(data => {
//     obj = data;
// })
/////////////////////////////////////////////////////////////////////////////////////////////////
////////////// Using pre-loaded json data in place of hitting the API ///////////////////////////
fs.readFile(`uniJSON.json`, `utf8`, (err, data) => {
    if (err) {
        console.log(err)
    } else {
        univ = JSON.parse(data)
    }
})
//////////////////////////////////////////////////////////////////////////////////////////////////


///////////// Get the yelp information for restaurants nearby all of the univerities return in the results above //////////////////////////////
let rests;
////////////// Use this section when hitting the API with registered key ////////////////////////
// const yelp = require(`./yelpSearch`)
// yelp(data => {
//     rests = data;
// })
/////////////////////////////////////////////////////////////////////////////////////////////////
////////////// Using pre-loaded json data in place of hitting the API ///////////////////////////
fs.readFile(`ratingData.json`, `utf8`, (err, data) => {
    if (err) {
        console.log(err)
    } else {
        rests = JSON.parse(data)
    }
})
//////////////////////////////////////////////////////////////////////////////////////////////////


app.use(express.static(`public`))

app.get(`/univ`, (req, res) => {
    res.json(univ)
});

app.get(`/yelp`, (req, res) => {
    res.json(rests)
});

app.listen(port)
