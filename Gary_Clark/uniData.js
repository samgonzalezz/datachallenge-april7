`use strict`

const fs = require(`fs`);
const jsonfile = require(`jsonfile`);
const _ = require(`lodash`);
const request = require(`request`);

module.exports = callback => {

    const key = `Register to get a key`

    // Make this URL request to get the data, need a key for this to work
    let TX = `https://api.data.gov/ed/collegescorecard/v1/schools.json?api_key=${key}&school.state=TX&school.carnegie_size_setting=12,13,14,15,16,17&school.main_campus=1&fields=school.name,school.city,location.lat,location.lon,2014.student.size&_page=0&_per_page=50`

    let optionsInit = {
        url: TX
    }

    request(optionsInit, (err, res, body) => {
        if (err) {
            console.log(err)
        } else {
            let data = JSON.parse(body)
            let dataSort = _.orderBy(data.results, [`2014.student.size`], [`desc`])
            let dataSlice = _.slice(dataSort, 0, 5)
            jsonfile.writeFile(`uniJSON.json`, dataSlice, { spaces: 2 }, err => {
                if (err) {
                    console.log(err)
                } else {
                    callback(dataSlice)
                }
            })
            
        }
    })
}