document.addEventListener("DOMContentLoaded", async () => {

    const respLaunchByDay = await fetch('./day.php');
    const launchByDay = await respLaunchByDay.json();

    const respLaunchByWeek = await fetch('./week.php');
    const launchByWeek = await respLaunchByWeek.json();
    launchByWeek.sort(weekSort);

    const respLaunchByMonth = await fetch('./month.php');
    const launchByMonth = await respLaunchByMonth.json();
    launchByMonth.sort(monthSort);

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

    // series colors, same as the former Highcharts default palette
    const COLORS = ['#2caffe', '#544fc5', '#00e272'];
    let chart = null;

    form.day.classList.add("active");
    drawChart("Days");

    function drawChart(period) {
        let dataByPeriod = countPeriod(period);

        if (chart) {
            chart.destroy();
        }
        out.innerHTML = "<canvas></canvas>";
        chart = new Chart(out.querySelector("canvas"), {
            data: {
                labels: dataByPeriod.categories,
                datasets: [{
                    type: 'bar',
                    label: 'Desktop',
                    data: dataByPeriod.desktop_users,
                    backgroundColor: COLORS[0],
                    order: 1
                }, {
                    type: 'bar',
                    label: 'Cloud',
                    data: dataByPeriod.cloud_users,
                    backgroundColor: COLORS[1],
                    order: 1
                }, {
                    type: 'line',
                    label: 'New',
                    data: dataByPeriod.new_users,
                    borderColor: COLORS[2],
                    backgroundColor: 'white',
                    pointBorderWidth: 2,
                    pointRadius: 4,
                    tension: 0.4,
                    order: 0  // drawn over the bars
                }]
            },
            options: {
                maintainAspectRatio: false,
                interaction: {
                    mode: 'index',
                    intersect: false
                },
                scales: {
                    x: {
                        grid: { display: false }
                    },
                    y: {
                        title: { display: true, text: 'Values' }
                    }
                },
                plugins: {
                    title: {
                        display: true,
                        text: dataByPeriod.period_name + ' Chart',
                        font: { size: 18 }
                    },
                    legend: {
                        position: 'bottom',
                        labels: {
                            usePointStyle: true,
                            // keep dataset order (Desktop, Cloud, New) instead of drawing order
                            sort: (a, b) => a.datasetIndex - b.datasetIndex
                        }
                    }
                }
            }
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

    function weekSort(date1, date2) {
        year1 = date1.year;
        week1 = date1.week;

        year2 = date2.year;
        week2 = date2.week;

        if (year1 === year2) {
            if (week1 > week2) {
                return 1;
            }

            if (week1 < week2) {
                return -1;
            }

            return 0;
        } else {
            if (year1 > year2) {
                return 1;
            } else {
                return -1;
            }
        }
    }

    function monthSort(date1, date2) {
        month1 = date1.month.split('.')[0];
        year1 = date1.month.split('.')[1];

        month2 = date2.month.split('.')[0];
        year2 = date2.month.split('.')[1];

        if (year1 === year2) {
            if (month1 > month2) {
                return 1;
            }

            if (month1 < month2) {
                return -1;
            }

            return 0;
        } else {
            if (year1 > year2) {
                return 1;
            } else {
                return -1;
            }
        }
    }
})