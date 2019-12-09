document.addEventListener("DOMContentLoaded", async () => {

    const respLaunchByDay = await fetch('./day.php');
    const launchByDay = await respLaunchByDay.json();

    const respLaunchByWeek = await fetch('./week.php');
    const launchByWeek = await respLaunchByWeek.json();

    const respLaunchByMonth = await fetch('./month.php');
    const launchByMonth = await respLaunchByMonth.json();

    let form = document.querySelector("form")
    let out = form.querySelector("#out")

    form.day.addEventListener("click", () => {
        out.innerHTML = "";
        form.day.classList.add("active");
        form.week.classList.remove("active");
        form.month.classList.remove("active");
        if (launchByDay.length === 0) {
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
        if (launchByWeek.length === 0) {
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
        if (launchByMonth.length === 0) {
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

            for (let i = 1; i < 32; i++) {
                if (i > launchByDay.length) break;
                categoriesArr.unshift(launchByDay[launchByDay.length - i].date);
                newUsersData.unshift(parseInt(launchByDay[launchByDay.length - i].new_users, 10));
                cloudUsersData.unshift(parseInt(launchByDay[launchByDay.length - i].cloud_launch, 10));
                desctopUsersData.unshift(parseInt(launchByDay[launchByDay.length - i].desktop_launch, 10));
            }
        }

        if (period === "Week") {
            periodName = "Weekly";

            for (let i = 1; i < 32; i++) {
                if (i > launchByWeek.length) break;
                categoriesArr.unshift(launchByWeek[launchByWeek.length - i].week);
                newUsersData.unshift(parseInt(launchByWeek[launchByWeek.length - i].new_users, 10));
                cloudUsersData.unshift(parseInt(launchByWeek[launchByWeek.length - i].cloud_launch, 10));
                desctopUsersData.unshift(parseInt(launchByWeek[launchByWeek.length - i].desktop_launch, 10));
            }
        }

        if (period === "Month") {
            periodName = "Monthly";

            for (let i = 1; i < 32; i++) {
                if (i > launchByMonth.length) break;
                categoriesArr.unshift(launchByMonth[launchByMonth.length - i].month);
                newUsersData.unshift(parseInt(launchByMonth[launchByMonth.length - i].new_users, 10));
                cloudUsersData.unshift(parseInt(launchByMonth[launchByMonth.length - i].cloud_launch, 10));
                desctopUsersData.unshift(parseInt(launchByMonth[launchByMonth.length - i].desktop_launch, 10));
            }
        }

        return { period_name: periodName, categories: categoriesArr, new_users: newUsersData, cloud_users: cloudUsersData, desktop_users: desctopUsersData }
    }

})