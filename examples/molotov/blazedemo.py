import molotov


@molotov.scenario(100)
async def scenario_one(session):
    async with session.get('https://blazedemo.com/') as resp:
        assert resp.status == 200
