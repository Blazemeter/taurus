document.addEventListener("DOMContentLoaded", async () => {

    const respNewUsers = await fetch('./new_users_by_day.json');
    const newUsers = await respNewUsers.json();

    const respLaunchByDay = await fetch('./launch_by_day.json');
    const launchByDay = await respLaunchByDay.json();

    const respLaunchByWeek = await fetch('./launch_by_week.json');
    const launchByWeek = await respLaunchByWeek.json();

    const respLaunchByMonth = await fetch('./launch_by_month.json');
    const launchByMonth = await respLaunchByMonth.json();

    let form = document.querySelector("form")
    let out = form.querySelector("#out")

    form.day.addEventListener("click", () => {
        out.innerHTML = "";
        form.day.classList.add("active");
        form.week.classList.remove("active");
        form.month.classList.remove("active");
        drawChart("Days");
    })

    form.week.addEventListener("click", () => {
        out.innerHTML = "";
        form.week.classList.add("active");
        form.day.classList.remove("active");
        form.month.classList.remove("active");
        drawChart("Week");
    })

    form.month.addEventListener("click", () => {
        out.innerHTML = "";
        form.month.classList.add("active");
        form.day.classList.remove("active");
        form.week.classList.remove("active");
        drawChart("Month");
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
        let currentYear = new Date().getFullYear();
        let lastMonth = new Date().getMonth();

        if (period === "Days") {
            periodName = "Daily";

            Object.keys(newUsers).sort().forEach(function (date) {
                if (Number(date.split('.')[1]) === lastMonth) {
                    categoriesArr.push(date);
                    newUsersData.push(newUsers[date]);
                    cloudUsersData.push(launchByDay[date].cloud_count);
                    desctopUsersData.push(launchByDay[date].desktop_count);
                }
            });
        }

        if (period === "Week") {
            periodName = "Weekly";

            Object.keys(launchByWeek[currentYear]).sort((a, b) => a-b).forEach(function (week) {
                categoriesArr.push(week);
                newUsersData.push(launchByWeek[currentYear][week].new_users_count);
                cloudUsersData.push(launchByWeek[currentYear][week].cloud_count);
                desctopUsersData.push(launchByWeek[currentYear][week].desktop_count);
            });
        }

        if (period === "Month") {
            periodName = "Monthly";

            Object.keys(launchByMonth).sort((a, b)=>a-b).forEach(function (month) {
                categoriesArr.push(month);
                newUsersData.push(launchByMonth[month].new_users_count);
                cloudUsersData.push(launchByMonth[month].cloud_count);
                desctopUsersData.push(launchByMonth[month].desktop_count);
            });
        }

        return { period_name: periodName, categories: categoriesArr, new_users: newUsersData, cloud_users: cloudUsersData, desktop_users: desctopUsersData }
    }

})