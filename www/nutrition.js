let kgToPound = 2.20462;
let apiKey = "&apiKey=f7296526052f4479bf17bd90e171cef1";


const form = new Vue({
    el: '#index',
    created: function () {
        this.show = true;
    },
    data: {
        show: true,

        age: 23,
        heightFeet: 5,
        heightInch: 5,
        activityLevel: "0",
        male: true,
        weight: 165,
        error: ""
    },
    methods: {
        selectGender: function (male) {
            this.male = male;
        },
        calc: function () {
            if (this.age == null || this.heightInch == null || this.heightInch == null || this.activityLevel == null) {
                this.error = "Please fill the form completely";
                return;
            }
            list.calculate(this.age, this.weight, this.heightFeet, this.heightInch, this.activityLevel, this.male);
            this.show = false;
        }
    }
});

const list = new Vue({
    el: '#list',
    created: function () {
        this.show = false
    },
    data: {
        show: false,
        calculatedCalories: 0,
        targetCalories: 0,
        weight: 0,
        targetWeight: 0,
        targetDays: 0,
        breakfastCal: 0,
        lunchCal: 0,
        dinnerCal: 0,

        breakfastItems: [],
        lunchItems: [],
        dinnerItems: []
    },
    methods: {
        calculate: function (age, weight, hFeet, hInch, activityLevel, male) {
            /*

            Male:	56.2 kg + 1.41 kg per inch over 5 feet
            Female:	53.1 kg + 1.36 kg per inch over 5 feet

            */

            /*
            * For men:
                BMR = 13.397W + 4.799H - 5.677A + 88.362
              For women:
                BMR = 9.247W + 3.098H - 4.330A + 447.593*/

            let totalHeight = (parseInt(hFeet) * 12.00) + parseInt(hInch);
            let compensation = activityLevel === "0" ? 1.2 : activityLevel === "1" ? 1.5 : 1.95;
            this.weight = parseInt(weight);
            this.calculatedCalories = male ? 13.397*this.weight + 4.799*totalHeight - 5.677*parseInt(age) + 88.362 : 9.247*this.weight + 3.098*totalHeight - 4.330*parseInt(age) + 447.593;

            this.calculatedCalories *= compensation;


            this.targetWeight = male ? (56.2 * kgToPound) + (1.41 * kgToPound) * (totalHeight - 60.00) : (53.1 * kgToPound) + (1.36 * kgToPound) * (totalHeight - 60.00);

            let weightDiff = this.weight - this.targetWeight;

            let diffCalorie = weightDiff * 3500;
            this.targetDays = diffCalorie/500;

            if(this.targetWeight < this.weight)
                this.targetCalories = this.calculatedCalories - 500;
            else
                this.targetCalories = this.calculatedCalories + 500;
            //if(weightDiff)

            let proportion = this.targetCalories / 6;
            this.breakfastCal = proportion * 2;
            this.lunchCal = proportion * 3;
            this.dinnerCal = proportion *1;

            //get breakfasts
            axios.get(`https://api.spoonacular.com/recipes/complexSearch?type=breakfast&maxCalories=${this.breakfastCal}&number=15${apiKey}`)
                .then(function (response) {
                    // handle success
                    console.log(response.data);
                    list.breakfastItems= response.data.results;
                }).catch((e=>{
                    console.log(e)
            }))


            //get lunch
            axios.get(`https://api.spoonacular.com/recipes/complexSearch?type=lunch&maxCalories=${this.lunchCal}&number=15${apiKey}`)
                .then(function (response) {
                    // handle success
                    console.log(response.data);
                    list.lunchItems= response.data.results;
                }).catch((e=>{
                console.log(e)
            }))


            //get dinner
            axios.get(`https://api.spoonacular.com/recipes/complexSearch?type=dinner&maxCalories=${this.dinnerCal}&number=15${apiKey}`)
                .then(function (response) {
                    // handle success
                    console.log(response.data);
                    list.dinnerItems= response.data.results;
                }).catch((e=>{
                console.log(e)
            }))


            this.show = true;
        },

        getRecipe(id) {
            details.getRecipe(id);
        }

    }
});

const details = new Vue({
    el: '#details',
    created: function()
    {
        this.show = false;
    },
    data: {
        show: false,
        recipe: {}
    },
    methods: {
        getRecipe(id)
        {
            axios.get(`https://api.spoonacular.com/recipes/${id}/information?includeNutrition=true${apiKey}`)
                .then(function (response) {
                    // handle success
                    console.log(response.data);
                    details.recipe = response.data;
                    list.show = false;
                    details.show = true;
                }).catch((e=>{
                console.log(e)
            }))
        },
        reset(){
            form.show = true;
            this.show = false;
        }
    }
})