from Chip import OpCodeDefinitions
from Tests.OpCodeTests.OpCodeTestBase import OpCodeTestBase


class TestRtiOpCode(OpCodeTestBase):
    def test_execute_rti_implied_command_calls_and_method(self):
        self.assert_opcode_execution(OpCodeDefinitions.rti_implied_command, self.target.get_rti_command_executed)
