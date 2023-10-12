import { ExternalLink, Link } from '@crowdstrike/ember-oss-docs';

export const Footer = <template>
  <footer class="bg-mezzanine theme-mezzanine pb-10 sm:pb-20 md:pb-28 pt-6 sm:pt-10 md:pt-20">
    <div class="max-w-screen-lg mx-auto grid gap-4 md:gap-14">
      <nav class="p-8 md:p-0 grid sm:flex flex-wrap justify-between">
        <div class="flex flex-col items-start">
          <Link @variant="quiet" @href="https://www.crowdstrike.com/why-crowdstrike/">Why CrowdStrike</Link>
          <Link @variant="quiet" @href="https://www.crowdstrike.com/why-crowdstrike/crowdstrike-customers/">Our Customers</Link>
        </div>
        <div class="flex flex-col items-start">
          <Link @variant="quiet" @href="https://www.crowdstrike.com/about-crowdstrike/">CrowdStrike's Story</Link>
          <Link @variant="quiet" @href="http://www.crowdstrike.com/news/">CrowdStrike News and Releases</Link>
        </div>
        <div class="flex flex-col items-start">
          <Link @variant="quiet" @href="https://www.crowdstrike.com/blog/category/engineering-and-technology/">CrowdStrike Engineering and Tech Blog CrowdStrike</Link>
          <Link @variant="quiet" @href="#">CrowdStrike People and Culture</Link>
          <Link @variant="quiet" @href="https://crowdstrike.wd5.myworkdayjobs.com/crowdstrikecareers">CrowdStrike Open Positions</Link>
        </div>
      </nav>

      <div class="p-8 md:p-0 grid gap-4 md:grid-flow-col w-full items-center">
        <ExternalLink @href="https://crowdstrike.com" class="mt-3 justify-self-start">
          <img src="/logo_footer.png" alt="Visit crowdstrike.com" />
        </ExternalLink>

        <div class="md:justify-self-center text-body-and-labels type-xs">
          <span class="px-2">Copyright &copy; 2023</span>
          <span>|</span>
          <ExternalLink @href="https://www.crowdstrike.com/contact-us/">Contact Us</ExternalLink>
          <span>|</span>
          <ExternalLink @href="https://www.crowdstrike.com/privacy-notice/">Private</ExternalLink>
          <span>|</span>
          <ExternalLink @href="https://www.crowdstrike.com/website-terms-of-use/">Terms of Use</ExternalLink>
          <span>|</span>
          <ExternalLink @href="https://www.crowdstrike.com/careers/candidate-privacy-notices/">Candidate Privacy Notices</ExternalLink>
        </div>

        <div class="md:justify-self-end flex gap-2 items-center">
          <ExternalLink @href="https://www.youtube.com/user/CrowdStrike">
            <img src="/youtube.png" alt="Visit the CrowdStrike YouTube channel" />
          </ExternalLink>
          <ExternalLink @href="https://www.instagram.com/crowdstrike/?hl=en">
            <img src="/instagram.png" alt="Visit the CrowdStrike Instagram" />
          </ExternalLink>
          <ExternalLink @href="https://www.linkedin.com/company/crowdstrike">
            <img src="/linkedin.png" alt="Visit the CrowdStrike LinkedIN" />
          </ExternalLink>
        </div>
      </div>
    </div>
  </footer>
</template>

export default Footer;
