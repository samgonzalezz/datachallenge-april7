`use strict`

const _ = require(`lodash`);
const yelp = require(`yelp-fusion`);
const Token = require(`./yelpToken.js`);
const jsonfile = require(`jsonfile`);

const uni = require(`./uniData`);

module.exports = callback => {
    let fullSet = [];
    uni(unis => {
        for (i = 0; i < unis.length; i++) {
            let lat = unis[i][`location.lat`]
            let lon = unis[i][`location.lon`]
            Token(token => {
                const client = yelp.client(token);
                client.search({
                    latitude: lat,
                    longitude: lon,
                    // location: `${zip}`,
                    categories: "food",
                    limit: 50
                }).then(response => {
                    let filt = _.filter(response.jsonBody.businesses, x => {
                        return x.rating >= 3
                    })
                    let fullFilt = _.map(filt, x => {
                        return {
                            name: x.name,
                            rating: x.rating,
                            lat: x.coordinates.latitude,
                            lng: x.coordinates.longitude
                        }
                    })
                    fullSet.push(fullFilt)
                    // console.log(fullSet.length)
                    if (fullSet.length == unis.length) {
                        let fullFlat = _.flatten(fullSet)
                        callback(fullFlat)
                        jsonfile.writeFile(`ratingData.json`, fullFlat, { spaces: 2 }, err => {
                            if (err) {
                                console.log(err)
                            } else {
                                // callback(fullFlat)
                            }
                        })
                    }
                }).catch(e => {
                    console.log(e)
                })
            })
        }

    })
}

