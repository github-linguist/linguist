/*
 * This file is part of PIC
 * Copyright © 2012 Rachel Mant (dx-mon@users.sourceforge.net)
 *
 * PIC is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * PIC is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "Memory.h"

class Memory16F88 : public Memory
{
private:
	uint8_t memory[512];
	std::map<uint32_t, MemoryLocation *> memoryMap;

public:
	Memory16F88();
	uint8_t Dereference(uint8_t bank, uint8_t partialAddress);
	uint8_t *Reference(uint8_t bank, uint8_t partialAddress);
	uint8_t *operator [](uint32_t ref);
};
