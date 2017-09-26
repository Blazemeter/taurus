import molotov


@molotov.scenario(100)
async def scenario_one(session):
    async with session.get('http://blazedemo.com/') as resp:
        assert resp.status == 200
        body = await resp.text()
        assert "Welcome" in body
