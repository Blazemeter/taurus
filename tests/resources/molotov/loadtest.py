import molotov


@molotov.scenario(100)
async def scenario_one(session):
    async with session.get('http://blazedemo.com/') as resp:
        assert resp.status == 200


@molotov.scenario(100)
async def scenario_two(session):
    async with session.get('http://blazedemo.com/not-found') as resp:
        assert resp.status == 404
