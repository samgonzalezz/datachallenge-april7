`use strict`

const yelp = require(`yelp-fusion`);
const yelpInfo = {
    id: `Register with Yelp to get this info`,
    secret: `Register with Yelp to get this info`
}

module.exports = callback => {
    const token = yelp.accessToken(yelpInfo.id, yelpInfo.secret).then(response => {
        let token = response.jsonBody.access_token;
        callback(token);
    }).catch(e => {
        console.log(e)
        callback(e)
    })
}