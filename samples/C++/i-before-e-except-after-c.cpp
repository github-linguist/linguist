#include <iostream>
#include <fstream>
#include <string>
#include <tuple>
#include <vector>
#include <stdexcept>
#include <boost/regex.hpp>



struct Claim {
        Claim(const std::string& name) : name_(name), pro_(0), against_(0), propats_(), againstpats_() {
        }

        void add_pro(const std::string& pat) {
               propats_.push_back(std::make_tuple(boost::regex(pat), pat[0] == '^'));
        }
        void add_against(const std::string& pat) {
               againstpats_.push_back(std::make_tuple(boost::regex(pat), pat[0] == '^'));
        }
        bool plausible() const { return pro_ > against_*2; }
        void check(const char * buf, uint32_t len) {
                for (auto i = propats_.begin(), ii = propats_.end(); i != ii; ++i) {
                        uint32_t pos = 0;
                        boost::cmatch m;
                        if (std::get<1>(*i) && pos > 0) continue;
                        while (pos < len && boost::regex_search(buf+pos, buf+len, m, std::get<0>(*i))) {
                                ++pro_;
                                if (pos > 0) std::cerr << name_ << " [pro] multiple matches in: " << buf << "\n";
                                pos += m.position() + m.length();
                        }
                }
                for (auto i = againstpats_.begin(), ii = againstpats_.end(); i != ii; ++i) {
                        uint32_t pos = 0;
                        boost::cmatch m;
                        if (std::get<1>(*i) && pos > 0) continue;
                        while (pos < len && boost::regex_search(buf+pos, buf+len, m, std::get<0>(*i))) {
                                ++against_;
                                if (pos > 0) std::cerr << name_ << " [against] multiple matches in: " << buf << "\n";
                                pos += m.position() + m.length();
                        }
                }
        }
        friend std::ostream& operator<<(std::ostream& os, const Claim& c);
private:
        std::string name_;
        uint32_t pro_;
        uint32_t against_;
        // tuple<regex,begin only>
        std::vector<std::tuple<boost::regex,bool>> propats_;
        std::vector<std::tuple<boost::regex,bool>> againstpats_;
};

std::ostream& operator<<(std::ostream& os, const Claim& c) {
        os << c.name_ << ": matches: " << c.pro_ << " vs. counter matches: " << c.against_ << ". ";
        os << "Plausibility: " << (c.plausible() ? "yes" : "no") << ".";
        return os;
}


int main(int argc, char ** argv) {
        try {
                if (argc < 2) throw std::runtime_error("No input file.");
                std::ifstream is(argv[1]);
                if (! is) throw std::runtime_error("Input file not valid.");

                Claim ieclaim("[^c]ie");
                ieclaim.add_pro("[^c]ie");
                ieclaim.add_pro("^ie");
                ieclaim.add_against("[^c]ei");
                ieclaim.add_against("^ei");

                Claim ceiclaim("cei");
                ceiclaim.add_pro("cei");
                ceiclaim.add_against("cie");

                {
                        const uint32_t MAXLEN = 32;
                        char buf[MAXLEN];
                        uint32_t longest = 0;
                        while (is) {
                                is.getline(buf, sizeof(buf));
                                if (is.gcount() <= 0) break;
                                else if (is.gcount() > longest) longest = is.gcount();
                                ieclaim.check(buf, is.gcount());
                                ceiclaim.check(buf, is.gcount());
                        }
                        if (longest >= MAXLEN) throw std::runtime_error("Buffer too small.");
                }

                std::cout << ieclaim << "\n";
                std::cout << ceiclaim << "\n";
                std::cout << "Overall plausibility: " << (ieclaim.plausible() && ceiclaim.plausible() ? "yes" : "no") << "\n";


        } catch (const std::exception& ex) {
                std::cerr << "*** Error: " << ex.what() << "\n";
                return -1;
        }
        return 0;
}
