document.addEventListener("DOMContentLoaded", async () => {

    const respLaunchByDay = await fetch('./stats_by_day.json');
    const launchByDay = await respLaunchByDay.json();

    const respLaunchByWeek = await fetch('./stats_by_week.json');
    const launchByWeek = await respLaunchByWeek.json();

    const respLaunchByMonth = await fetch('./stats_by_month.json');
    const launchByMonth = await respLaunchByMonth.json();

    let form = document.querySelector("form")
    let out = form.querySelector("#out")

    form.day.addEventListener("click", () => {
        out.innerHTML = "";
        form.day.classList.add("active");
        form.week.classList.remove("active");
        form.month.classList.remove("active");
        if(launchByDay.length === 0){
            out.innerHTML = "<p class='no-data'>No Data</p>";
        } else {
            drawChart("Days");
        }
    })

    form.week.addEventListener("click", () => {
        out.innerHTML = "";
        form.week.classList.add("active");
        form.day.classList.remove("active");
        form.month.classList.remove("active");
        if(launchByWeek.length === 0){
            out.innerHTML = "<p class='no-data'>No Data</p>";
        } else {
            drawChart("Week");
        }
    })

    form.month.addEventListener("click", () => {
        out.innerHTML = "";
        form.month.classList.add("active");
        form.day.classList.remove("active");
        form.week.classList.remove("active");
        if(launchByMonth.length === 0){
            out.innerHTML = "<p class='no-data'>No Data</p>";
        } else {
            drawChart("Month");
        }
    })

    form.day.classList.add("active");
    drawChart("Days");

    function drawChart(period) {
        let dataByPeriod = countPeriod(period);

        Highcharts.chart('out', {
            title: {
                text: dataByPeriod.period_name + ' Chart'
            },
            xAxis: {
                categories: dataByPeriod.categories
            },
            series: [{
                type: 'column',
                name: 'Desktop',
                data: dataByPeriod.desktop_users
            }, {
                type: 'column',
                name: 'Cloud',
                data: dataByPeriod.cloud_users
            }, {
                type: 'spline',
                name: 'New',
                data: dataByPeriod.new_users,
                marker: {
                    lineWidth: 2,
                    lineColor: Highcharts.getOptions().colors[2],
                    fillColor: 'white'
                }
            }]
        });
    }

    function countPeriod(period) {
        let categoriesArr = [];
        let newUsersData = [];
        let cloudUsersData = [];
        let desctopUsersData = [];
        let periodName = "";

        if (period === "Days") {
            periodName = "Daily";

            for(let i = 1; i < 32; i++) {
                if (i > launchByDay.length) break;
                categoriesArr.unshift(launchByDay[launchByDay.length - i].date);
                newUsersData.unshift(launchByDay[launchByDay.length - i].new_users);
                cloudUsersData.unshift(launchByDay[launchByDay.length - i].cloud_launch);
                desctopUsersData.unshift(launchByDay[launchByDay.length - i].desktop_launch);
            }
        }

        if (period === "Week") {
            periodName = "Weekly";

            for(let i = 1; i < 32; i++) {
                if (i > launchByWeek.length) break;
                categoriesArr.unshift(launchByWeek[launchByWeek.length - i].week);
                newUsersData.unshift(launchByWeek[launchByWeek.length - i].new_users);
                cloudUsersData.unshift(launchByWeek[launchByWeek.length - i].cloud_launch);
                desctopUsersData.unshift(launchByWeek[launchByWeek.length - i].desktop_launch);
            }
        }

        if (period === "Month") {
            periodName = "Monthly";

            for(let i = 1; i < 32; i++) {
                if (i > launchByMonth.length) break;
                categoriesArr.unshift(launchByMonth[launchByMonth.length - i].month);
                newUsersData.unshift(launchByMonth[launchByMonth.length - i].new_users);
                cloudUsersData.unshift(launchByMonth[launchByMonth.length - i].cloud_launch);
                desctopUsersData.unshift(launchByMonth[launchByMonth.length - i].desktop_launch);
            }
        }

        return { period_name: periodName, categories: categoriesArr, new_users: newUsersData, cloud_users: cloudUsersData, desktop_users: desctopUsersData }
    }

})